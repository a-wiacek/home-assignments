#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <signal.h>
#include <pthread.h>

#include "const.h"
#include "sender.h"
#include "err.h"

// Variables used by multiple threads.
// They can only change in main.

struct sockaddr_in ctrl_recv_addr, data_addr;
char mcast_address[ADDRESS_BUFFER_SIZE],
     sender_name[SENDER_NAME_MAX_SIZE] = INITIAL_SENDER_NAME;
uint16_t data_port = INITIAL_DATA_PORT,
         control_port = INITIAL_CONTROL_PORT;
unsigned int pack_size = INITIAL_PACK_SIZE,
             fifo_size = INITIAL_FIFO_SIZE,
             retransmission_time = INITIAL_RETRANSMISSION_TIME;
uint64_t session_id;

// Data shared by threads, threads implementation.

pthread_t data_thread, control_thread, rexmit_thread;

struct rexmit_t {
    struct rexmit_t *next;
    int pack_num;
};

struct sender_data_t {
    struct audio_pack *data_queue;
    int data_sock, control_recv_sock, control_send_sock,
        data_queue_position;
    uint32_t packs_made;
    struct rexmit_t *rexmit_list, *rexmit_last;
    pthread_mutex_t lock_data, lock_rexmit;
};
struct sender_data_t sender_data;

void free_rexmit(struct rexmit_t *pointer)
{
    if (pointer == NULL) return;
    free_rexmit(pointer->next);
    free(pointer);
}

char *send_data; // Needs to be global so we can free it in signal handler.
void *send_data_to_receiver(void *args)
{
    int i;
    size_t read_bytes, to_read, single_read_bytes;
    send_data = malloc(sizeof(char) * (pack_size + AUDIO_PACK_INFO_SIZE));
    uint64_t hton_temp; // Host to network temporary place.
    if (send_data == NULL)
        SIG_SELF;
    while (1) {
        read_bytes = 0;
        to_read = pack_size;
        while (read_bytes < pack_size) {
            single_read_bytes = read(STDIN_FILENO,
					send_data + AUDIO_PACK_INFO_SIZE + read_bytes, to_read);
            if (single_read_bytes < 0)
                SIG_SELF;
			if (single_read_bytes == 0) // EOF
                return ((void *) 0);
            to_read -= single_read_bytes;
            read_bytes += single_read_bytes;
		}
        if (pthread_mutex_lock(&sender_data.lock_data) < 0)
            SIG_SELF;
        i = sender_data.data_queue_position;
        memset(sender_data.data_queue[i].audio_data, 0, pack_size);
        memcpy(sender_data.data_queue[i].audio_data,
               send_data + AUDIO_PACK_INFO_SIZE, pack_size);
        sender_data.data_queue[i].first_byte_num =
                sender_data.packs_made * pack_size;
        sender_data.data_queue[i].session_id = session_id;
        hton_temp = htonll(sender_data.data_queue[i].session_id);
        memcpy(send_data, &hton_temp, sizeof(uint64_t));
        hton_temp = htonll(sender_data.data_queue[i].first_byte_num);
        memcpy(send_data + sizeof(uint64_t), &hton_temp, sizeof(uint64_t));
        sender_data.data_queue_position++;
        // Next data pack won't fit here, move to the beginning of array.
        if (sender_data.data_queue_position == fifo_size)
            sender_data.data_queue_position = 0;
        sender_data.packs_made++;
        if (pthread_mutex_unlock(&sender_data.lock_data) < 0 ||
            write(sender_data.data_sock, send_data,
                  pack_size + AUDIO_PACK_INFO_SIZE)
            < pack_size + AUDIO_PACK_INFO_SIZE)
            SIG_SELF;
    }
}

char *ctrl_temp_buffer; // Global for signal handler.
struct rexmit_t *ctrl_new_rexmit, *ctrl_last_rexmit;
void *control_broadcast(void *args)
{
    int pack_to_rexmit;
    ssize_t bytes_read, i, j;
    socklen_t recv_len = sizeof(struct sockaddr);
    struct sockaddr_in src_addr;
    ctrl_temp_buffer = malloc(sizeof(char) * TEMP_BUFFER_SIZE);
    if (ctrl_temp_buffer == NULL)
        SIG_SELF;
    while ((bytes_read = recvfrom(sender_data.control_recv_sock,
                  ctrl_temp_buffer, TEMP_BUFFER_SIZE, 0,
                  (struct sockaddr *)&src_addr, &recv_len)) > 0) {
        if (strcmp(ctrl_temp_buffer, LOOKUP_TEXT) == 0) { // Identify yourself
            memset(ctrl_temp_buffer, 0, TEMP_BUFFER_SIZE);
            i = strlen(REPLY_TEXT);
            j = 0;
            // Prepare message
            memcpy(ctrl_temp_buffer, REPLY_TEXT, strlen(REPLY_TEXT));
            while (mcast_address[j] != 0) // Copy mcast_addr and set i past it.
                ctrl_temp_buffer[i++] = mcast_address[j++];
            ctrl_temp_buffer[i++] = ' '; // Set i past space.
            i += sprintf(ctrl_temp_buffer + i, "%d ", data_port);
            i += sprintf(ctrl_temp_buffer + i, "%s\n", sender_name);
            if (sendto(sender_data.control_recv_sock, ctrl_temp_buffer, i, 0,
                       (struct sockaddr *)&src_addr, recv_len) < i)
                SIG_SELF;
        } else if (strncmp(ctrl_temp_buffer, REXMIT_TEXT, strlen(REXMIT_TEXT)) == 0) {
            // Save rexmit request to resend pack later on.
            ctrl_new_rexmit = malloc(sizeof(struct rexmit_t));
            ctrl_last_rexmit = ctrl_new_rexmit;
            if (ctrl_new_rexmit == NULL)
                SIG_SELF;
            i = strlen(REXMIT_TEXT);
            pack_to_rexmit = 0;
            while (ctrl_temp_buffer[i] != '\n') {
                if (ctrl_temp_buffer[i] == ',') {
                    ctrl_last_rexmit->pack_num = pack_to_rexmit;
                    ctrl_last_rexmit->next = malloc(sizeof(struct rexmit_t));
                    if (ctrl_last_rexmit->next == NULL)
                        SIG_SELF;
                    ctrl_last_rexmit = ctrl_last_rexmit->next;
                    pack_to_rexmit = 0;
                } else
                    pack_to_rexmit = 10 * pack_to_rexmit - '0'
                                     + ctrl_temp_buffer[i];
                ++i;
            }
            ctrl_last_rexmit->pack_num = pack_to_rexmit;
            if (pthread_mutex_lock(&sender_data.lock_rexmit) < 0)
                SIG_SELF;
            if (sender_data.rexmit_list == NULL) { // Starting new list.
                sender_data.rexmit_list = ctrl_new_rexmit;
                sender_data.rexmit_last = ctrl_last_rexmit;
            } else { // There are some elements already.
                sender_data.rexmit_last->next = ctrl_new_rexmit;
                sender_data.rexmit_last = ctrl_last_rexmit;
            }
            if (pthread_mutex_unlock(&sender_data.lock_rexmit) < 0)
                SIG_SELF;
        } // Otherwise unknown text, ignore it.
        memset(ctrl_temp_buffer, 0, TEMP_BUFFER_SIZE);
    }
    if (bytes_read < 0)
        SIG_SELF;
    return ((void *) 0);
}

char *rexmit_data; // Global for signal handler.
struct rexmit_t *rexmit_rexmit_list;
void *retransmition(void *args)
{
    struct timespec delay;
    delay.tv_sec = retransmission_time / 1000; // [s]
    delay.tv_nsec = (retransmission_time % 1000) * 1000000; // [ms] to [ns]
    struct rexmit_t *iterator;
    int i;
    rexmit_data = malloc(sizeof(char) * (pack_size + AUDIO_PACK_INFO_SIZE));
    if (rexmit_data == NULL)
        SIG_SELF;
    while(1) {
        nanosleep(&delay, NULL);
        if (pthread_mutex_lock(&sender_data.lock_rexmit) < 0)
            SIG_SELF;
        // Take list of rexmit requests.
        rexmit_rexmit_list = sender_data.rexmit_list;
        sender_data.rexmit_list = NULL;
        if (pthread_mutex_unlock(&sender_data.lock_rexmit) < 0 ||
            pthread_mutex_lock(&sender_data.lock_data) < 0)
            SIG_SELF;
        iterator = rexmit_rexmit_list;
        while (iterator != NULL) {
            // Check whether we have that pack.
            if (iterator->pack_num < sender_data.packs_made &&
                iterator->pack_num > 0 &&
                iterator->pack_num >= sender_data.packs_made - fifo_size) {
                memset(rexmit_data, 0, pack_size + AUDIO_PACK_INFO_SIZE);
                i = sender_data.data_queue_position + iterator->pack_num
                    - sender_data.packs_made;
                if (i < 0) i += fifo_size; // i mod fifo_size
                memcpy(rexmit_data, &sender_data.data_queue[i],
                       AUDIO_PACK_INFO_SIZE);
                memcpy(rexmit_data + AUDIO_PACK_INFO_SIZE,
                       sender_data.data_queue->audio_data, pack_size);
                if (write(sender_data.data_sock, rexmit_data,
                          pack_size + AUDIO_PACK_INFO_SIZE) <
                          pack_size + AUDIO_PACK_INFO_SIZE)
                    SIG_SELF;
            }
            iterator = iterator->next;
        }
        if (pthread_mutex_unlock(&sender_data.lock_data) < 0)
            SIG_SELF;
        free_rexmit(rexmit_rexmit_list);
    }
}

// Main and functions related to terminating program.

void input_error()
{
    fprintf(stderr, SENDER_USAGE_INFO);
    exit(1);
}

void clean()
{
    int i;
    if (sender_data.data_queue != NULL) {
        for (i = 0; i < fifo_size; ++i)
            free(sender_data.data_queue[i].audio_data);
        free(sender_data.data_queue);
    }
    pthread_mutex_destroy(&sender_data.lock_data);
    pthread_mutex_destroy(&sender_data.lock_rexmit);
    free_rexmit(sender_data.rexmit_list);
    free_rexmit(ctrl_new_rexmit);
    free_rexmit(rexmit_rexmit_list);
    FREE_SAFE(rexmit_data);
    FREE_SAFE(send_data);
    FREE_SAFE(ctrl_temp_buffer);
}

void handle_signal(int signal)
{
    fprintf(stderr, "Terminating sender\n");
    if (errno)
        fprintf(stderr, "Error %d: %s\n", errno, strerror(errno));
    clean();
    exit(1); // This exits all threads.
}

int main(int argc, char *argv[])
{
    // Handle input parameters.
    int option;
    unsigned int input_flags = 0, i, j;
    intmax_t from_strtol;

    while ((option = getopt(argc, argv, "a:P:C:p:f:R:n:")) != -1) {
        switch (option) {
            case 'a':
                if (input_flags & MCAST_ADDR_FLAG) // Option repeated.
                    input_error();
                input_flags |= MCAST_ADDR_FLAG;
                i = 0;
                while (*optarg != ' ')
                    mcast_address[i++] = *optarg++;
                mcast_address[i] = 0;
                break;
            case 'P':
                if (input_flags & DATA_PORT_FLAG)
                    input_error();
                input_flags |= DATA_PORT_FLAG;
                from_strtol = strtol(optarg, NULL, 10);
                if (errno == ERANGE ||
                    from_strtol <= 0 ||
                    from_strtol > UINT16_MAX) {
                    fprintf(stderr, "Invalid data port number\n");
                    exit(1);
                }
                data_port = (uint16_t) from_strtol;
                break;
            case 'C':
                if (input_flags & CONTROL_PORT_FLAG)
                    input_error();
                input_flags |= CONTROL_PORT_FLAG;
                from_strtol = strtol(optarg, NULL, 10);
                if (errno == ERANGE ||
                    from_strtol <= 0 ||
                    from_strtol > UINT16_MAX) {
                    fprintf(stderr, "Invalid control port number\n");
                    exit(1);
                }
                control_port = (uint16_t) from_strtol;
                break;
            case 'p':
                if (input_flags & PACK_SIZE_FLAG)
                    input_error();
                input_flags |= PACK_SIZE_FLAG;
                from_strtol = strtol(optarg, NULL, 10);
                if (errno == ERANGE ||
                    from_strtol <= 0) {
                    fprintf(stderr, "Invalid pack size\n");
                    exit(1);
                }
                pack_size = (unsigned int) from_strtol;
                break;
            case 'f':
                if (input_flags & FIFO_SIZE_FLAG)
                    input_error();
                input_flags |= FIFO_SIZE_FLAG;
                from_strtol = strtol(optarg, NULL, 10);
                if (errno == ERANGE ||
                    from_strtol <= 0) {
                    fprintf(stderr, "Invalid FIFO queue size\n");
                    exit(1);
                }
                fifo_size = (unsigned int) from_strtol;
                break;
            case 'R':
                if (input_flags & RETRANSMISSION_TIME_FLAG)
                    input_error();
                input_flags |= RETRANSMISSION_TIME_FLAG;
                from_strtol = strtol(optarg, NULL, 10);
                if (errno == ERANGE ||
                    from_strtol <= 0) {
                    fprintf(stderr, "Invalid retransmission value\n");
                    exit(1);
                }
                retransmission_time = (unsigned int) from_strtol;
                break;
            case 'n':
                if (input_flags & SENDER_NAME_FLAG)
                    input_error();
                input_flags |= SENDER_NAME_FLAG;
                strcpy(sender_name, optarg);
                break;
            default: // Unknown parameter, terminate program.
                input_error();
        }
    }

    if (!(input_flags & MCAST_ADDR_FLAG)) // This option is obligatory.
        input_error();

    // It would be great if fifo_size is multiple of pack_size.
    // In case it is not, just increase it.
    if (fifo_size % pack_size)
        fifo_size += pack_size - (fifo_size % pack_size);
    fifo_size /= pack_size;
    sender_data.packs_made = 0;
    sender_data.data_queue_position = 0;

    // Prepare for receiving signal.
    struct sigaction action;
    action.sa_handler = handle_signal;
    action.sa_flags = 0;
    if (sigaction(SIGTERM, &action, NULL) == -1)
        syserr("sender: sigaction");

    // Achieve connection:

    //  - to send data,
    sender_data.data_sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (sender_data.data_sock < 0)
        syserr("sender: socket");
    int broadcast = 1;
    if (setsockopt(sender_data.data_sock, SOL_SOCKET, SO_BROADCAST, &broadcast,
                   sizeof(broadcast)) == -1)
        syserr("sender: setsockopt (SO_BROADCAST)");
    data_addr.sin_family = AF_INET;
    data_addr.sin_port = htons(data_port);
    if (inet_aton(mcast_address, &data_addr.sin_addr) == 0)
        syserr("sender: inet_aton");
    if (connect(sender_data.data_sock, (struct sockaddr *)&data_addr,
                (socklen_t) sizeof(data_addr)) < 0)
        syserr("sender: connect");

    //  - to receive control data,
    sender_data.control_recv_sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (sender_data.control_recv_sock < 0)
        syserr("sender: socket");
    ctrl_recv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    ctrl_recv_addr.sin_port = htons(control_port);
    ctrl_recv_addr.sin_family = AF_INET;
    if (bind(sender_data.control_recv_sock, (struct sockaddr *)&ctrl_recv_addr,
             (socklen_t) sizeof(ctrl_recv_addr)) < 0)
        syserr("sender: bind");

    //  - to send control data.
    sender_data.control_send_sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (sender_data.control_send_sock < 0)
        syserr("sender: socket");

    // Allocate buffers:
    sender_data.data_queue = malloc(sizeof(struct audio_pack) * fifo_size);
    if (sender_data.data_queue == NULL)
        syserr("malloc");
    for (i = 0; i < fifo_size; ++i) {
        sender_data.data_queue[i].audio_data = malloc(sizeof(char) * pack_size);
        if (sender_data.data_queue[i].audio_data == NULL) {
            for (j = 0; j < i; ++j)
                free(sender_data.data_queue[j].audio_data);
            free(sender_data.data_queue);
            syserr("malloc");
        }
    }

    // Clear global variables:
    sender_data.data_queue_position = 0;
    sender_data.packs_made = 0;
    sender_data.rexmit_list = NULL;
    sender_data.rexmit_last = NULL;
    send_data = NULL;
    ctrl_temp_buffer = NULL;
    ctrl_new_rexmit = NULL;
    ctrl_last_rexmit = NULL;
    rexmit_data = NULL;
    rexmit_rexmit_list = NULL;
    session_id = time(NULL);


    // Prepare and start threads:
    if (pthread_mutex_init(&sender_data.lock_data, 0) ||
        pthread_mutex_init(&sender_data.lock_rexmit, 0)) {
        for (i = 0; i < fifo_size; ++i)
            free(sender_data.data_queue[i].audio_data);
        free(sender_data.data_queue);
        syserr("pthread_mutex_init");
    }
    pthread_attr_t thread_attr;
    if (pthread_attr_init(&thread_attr) ||
        pthread_create(&data_thread, &thread_attr, send_data_to_receiver, 0) ||
        pthread_create(&control_thread, &thread_attr, control_broadcast, 0) ||
        pthread_create(&rexmit_thread, &thread_attr, retransmition, 0)) {
        handle_signal(0);
    }

    // End program:
    void *return_value;
    pthread_join(data_thread, &return_value);
    pthread_cancel(control_thread);
    pthread_cancel(rexmit_thread);
    pthread_attr_destroy(&thread_attr);
    clean();
    return 0;
}
