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
#include <inttypes.h>

#include "const.h"
#include "receiver.h"
#include "err.h"

// Variables used by multiple threads.
// They can only change in main.

struct sockaddr_in ctrl_recv_addr, ctrl_send_addr;
char discover_address[ADDRESS_BUFFER_SIZE] = INITIAL_DISCOVER_ADDRESS,
     listento_name[SENDER_NAME_MAX_SIZE];
uint16_t control_port = INITIAL_CONTROL_PORT,
         ui_port = INITIAL_UI_PORT;
int data_buffer_size = INITIAL_DATA_BUFFER_SIZE,
    retransmission_time = INITIAL_RETRANSMISSION_TIME;

// Data shared by threads, threads implementation.

pthread_t discover_thread, lookups_thread, clean_thread, ui_thread, data_thread;

struct radio_station_t {
    struct radio_station_t *prev;
    struct radio_station_t *next;
    time_t time; // When did I read last reply?
    char name[SENDER_NAME_MAX_SIZE]; // Names are saved with '\r\n'.
    char mcast_addr[ADDRESS_BUFFER_SIZE];
    uint16_t port;
};

struct receiver_data_t {
    int ctrl_sock,
        users_connected[MAX_USERS_CONNECTED],
        reading_arrow[MAX_USERS_CONNECTED];
    struct radio_station_t *first_radio_station, *selected_radio_station;
    pthread_mutex_t lock_radio_stations;
    pthread_cond_t cond_receiver;
};
struct receiver_data_t receiver_data;

struct rexmit_data_t {
    char *msg;
    char mcast_addr[ADDRESS_BUFFER_SIZE];
    uint16_t port;
};

struct rexmit_sender_t {
    pthread_t thread;
    char *message;
    struct rexmit_sender_t *next;
    struct rexmit_data_t *data;
};

void free_rexmit_sender(struct rexmit_sender_t *pointer)
{
    if (pointer == NULL) return;
    free_rexmit_sender(pointer->next);
    pthread_cancel(pointer->thread);
    free(pointer->message);
    free(pointer->data);
    free(pointer);
}

struct write_out_t {
    char *buffer, *temp_buffer;
    size_t pack_size;
    uint64_t byte0, session_id, filled_bytes,
             top_byte; // Biggest number of byte we read, all bytes below are
                       // either read or requested by rexmit.
    int data_sock;
    uint64_t *buffer_filled; // Numbers of bytes
    struct rexmit_sender_t *rexmit_sender_list, *rexmit_sender_last;
};
struct write_out_t write_out;

void write_list_of_stations_for_one_client(int fd)
{
    if (fd == 0) return;
    if (pthread_mutex_lock(&receiver_data.lock_radio_stations) < 0 ||
        clear_screen(fd) < 0 ||
        write(fd, RADIO_HEADER, strlen(RADIO_HEADER)) < strlen(RADIO_HEADER))
        SIG_SELF;
    struct radio_station_t *iterator = receiver_data.first_radio_station;
    if (iterator == NULL) {
        if (write(fd, NOTHING_UP, strlen(NOTHING_UP)) < strlen(NOTHING_UP) ||
            write(fd, END_LINE, strlen(END_LINE)) < strlen(END_LINE) ||
            pthread_mutex_unlock(&receiver_data.lock_radio_stations) < 0)
            SIG_SELF;
        return;
    }
    do {
        if (iterator == receiver_data.selected_radio_station) {
            if (write(fd, UI_PTR, strlen(UI_PTR)) < strlen(UI_PTR))
                SIG_SELF;
        } else if (write(fd, UI_NO_PTR, strlen(UI_NO_PTR)) < strlen(UI_NO_PTR))
            SIG_SELF;
        if (write(fd, iterator->name, strlen(iterator->name))
            < strlen(iterator->name))
            SIG_SELF;
        iterator = iterator->next;
    } while (iterator != NULL);
    if (write(fd, END_LINE, strlen(END_LINE)) < strlen(END_LINE) ||
        pthread_mutex_unlock(&receiver_data.lock_radio_stations) < 0)
        SIG_SELF;
}

void write_list_of_stations_for_all_clients()
{
    int i;
    for (i = 0; i < MAX_USERS_CONNECTED; ++i)
        write_list_of_stations_for_one_client(receiver_data.users_connected[i]);
}

void insert_radiostation(struct radio_station_t *station)
{
    // Reminder: thread has receiver_data.lock_radio_stations
    // and will release this mutex at the end.
    if (receiver_data.first_radio_station == NULL) {
        receiver_data.first_radio_station = station;
        receiver_data.selected_radio_station = station;
        pthread_cond_signal(&receiver_data.cond_receiver);
    } else {
        struct radio_station_t *iterator = receiver_data.first_radio_station;
        if (strcmp(station->name, iterator->name) <= 0) {
            receiver_data.first_radio_station = station;
            station->next = iterator;
            station->prev = NULL;
            iterator->prev = station;
            if (strcmp(station->name, listento_name) == 0)
                receiver_data.selected_radio_station = station;
        } else {
            while (iterator->next != NULL) {
                if (strcmp(station->name, iterator->next->name) <= 0) {
                    // We found proper place to insert station.
                    station->prev = iterator;
                    station->next = iterator->next;
                    iterator->next->prev = station;
                    iterator->next = station;
                    if (strcmp(station->name, listento_name) == 0)
                        receiver_data.selected_radio_station = station;
                    break;
                }
                iterator = iterator->next;
            }
            if (iterator->next == NULL) { // Insert past last.
                iterator->next = station;
                station->prev = iterator;
                station->next = NULL;
                if (strcmp(station->name, listento_name) == 0)
                    receiver_data.selected_radio_station = station;
            }
        }
    }
    if (pthread_mutex_unlock(&receiver_data.lock_radio_stations) < 0)
        SIG_SELF;
    write_list_of_stations_for_all_clients();
}

void remove_radiostation(struct radio_station_t *station)
{
    // Reminder: thread has receiver_data.lock_radio_stations.
    if (station->next != NULL)
        station->next->prev = station->prev;
    if (station->prev != NULL)
        station->prev->next = station->next;
    if (receiver_data.first_radio_station == station)
        receiver_data.first_radio_station = station->next;
        // This is OK also in case of NULL.
    if (receiver_data.selected_radio_station == station) {
        if (station->next != NULL)
            receiver_data.selected_radio_station = station->next;
        else // This could be NULL too, but we can't do anything more with that.
            receiver_data.selected_radio_station = station->prev;
        free_rexmit_sender(write_out.rexmit_sender_list);
        write_out.rexmit_sender_list = NULL;
    }
    free(station);
}

void *discover_radiostations(void *args)
{
    struct timespec delay;
    delay.tv_sec = LOOKUP_DELAY;
    delay.tv_nsec = 0;
    while (1) {
        if (sendto(receiver_data.ctrl_sock, LOOKUP_TEXT, strlen(LOOKUP_TEXT), 0,
                   (struct sockaddr *)&ctrl_send_addr, sizeof(ctrl_send_addr))
                   < strlen(LOOKUP_TEXT))
            SIG_SELF;
        nanosleep(&delay, NULL);
    }
}

char rl_temp_buffer[TEMP_BUFFER_SIZE], rl_mcast_addr[ADDRESS_BUFFER_SIZE];
void *receive_lookups(void *args)
{
    size_t i, j, mcaddr_size, station_exists;
    uint16_t port;
    struct radio_station_t *iterator, *new_radio_station;
    memset(rl_mcast_addr, 0, ADDRESS_BUFFER_SIZE);
    memset(rl_temp_buffer, 0, TEMP_BUFFER_SIZE);
    while (read(receiver_data.ctrl_sock, rl_temp_buffer, TEMP_BUFFER_SIZE) > 0) {
        // Message structure: Reply_text Mcast_addr Data_port Name\n
        if (strncmp(rl_temp_buffer, REPLY_TEXT, strlen(REPLY_TEXT)) != 0)
            continue; // Unknown message (propably LOOKUP_TEXT), ignore it.
        i = strlen(REPLY_TEXT);
        j = i;
        while (rl_temp_buffer[j] != ' ') ++j;
        mcaddr_size = j - i;
        memcpy(rl_mcast_addr, rl_temp_buffer + i, mcaddr_size);
        i = j + 1; // First digit of data port.
        port = 0;
        while (rl_temp_buffer[i] != ' ')
            port = 10 * port + rl_temp_buffer[i++] - '0';
        ++i; // Skip last space, get station name.
        j = i; // i points at first sign of name.
        while (rl_temp_buffer[j] != '\n')
            ++j;
        rl_temp_buffer[j++] = '\r'; // Insert '\r' before '\n'.
        rl_temp_buffer[j++] = '\n';
        // If station exists already, update its time.
        station_exists = 0;
        if (pthread_mutex_lock(&receiver_data.lock_radio_stations) < 0)
            SIG_SELF;
        iterator = receiver_data.first_radio_station;
        while (iterator != NULL) {
            // We identify station using tuple: (address, name, port).
            if (strncmp(iterator->name, rl_temp_buffer + i, j - i) == 0 &&
                strncmp(iterator->mcast_addr, rl_mcast_addr, mcaddr_size) == 0 &&
                iterator->port == port) {
                iterator->time = time(NULL);
                station_exists = 1;
                if (pthread_mutex_unlock(&receiver_data.lock_radio_stations) < 0)
                    SIG_SELF;
                break;
            }
            iterator = iterator->next;
        }
        if (!station_exists) { // Create new station.
            new_radio_station = malloc(sizeof(struct radio_station_t));
            if (new_radio_station == NULL)
                SIG_SELF;
            memset(new_radio_station, 0, sizeof(struct radio_station_t));
            new_radio_station->time = time(NULL);
            new_radio_station->port = port;
            strcpy(new_radio_station->mcast_addr, rl_mcast_addr);
            strcpy(new_radio_station->name, rl_temp_buffer + i);
            insert_radiostation(new_radio_station); // This will handle mutex.
        }
        memset(rl_mcast_addr, 0, ADDRESS_BUFFER_SIZE);
        memset(rl_temp_buffer, 0, TEMP_BUFFER_SIZE);
    }
    return ((void *) 0);
}

void *remove_inactives(void *args)
{
    // Check every 0,5 seconds for inactive stations to remove them from list.
    struct timespec delay;
    delay.tv_sec = 0;
    delay.tv_nsec = 500000000; // [ns]
    struct radio_station_t *iterator, *to_remove;
    time_t timer;
    int changed;
    while (1) {
        nanosleep(&delay, NULL);
        if (pthread_mutex_lock(&receiver_data.lock_radio_stations) < 0)
            SIG_SELF;
        changed = 0;
        iterator = receiver_data.first_radio_station;
        timer = time(NULL);
        while (iterator != NULL) {
            if (difftime(timer, iterator->time) > MAX_INACTIVE_TIME) {
                changed = 1;
                to_remove = iterator;
                iterator = iterator->next;
                remove_radiostation(to_remove);
            } else
                iterator = iterator->next;
        }
        if (pthread_mutex_unlock(&receiver_data.lock_radio_stations) < 0)
            SIG_SELF;
        if (changed)
            write_list_of_stations_for_all_clients();
    }
}

void process_input_char(char msg, int *reading_arrow)
{
    if (msg == ARROW_PREFIX[0])
        *reading_arrow = 1;
    else if (*reading_arrow == 1 && msg == ARROW_PREFIX[1])
        *reading_arrow = 2;
    else if (*reading_arrow == 2 && msg == UP_ARROW_SUFFIX) {
        *reading_arrow = 0;
        if (pthread_mutex_lock(&receiver_data.lock_radio_stations) < 0)
            SIG_SELF;
        if (receiver_data.selected_radio_station != NULL &&
            receiver_data.selected_radio_station->prev != NULL) {
            receiver_data.selected_radio_station =
                    receiver_data.selected_radio_station->prev;
            if (pthread_mutex_unlock(&receiver_data.lock_radio_stations) < 0)
                SIG_SELF;
            write_list_of_stations_for_all_clients();
        } else if (pthread_mutex_unlock(&receiver_data.lock_radio_stations) < 0)
            SIG_SELF;
    } else if (*reading_arrow == 2 && msg == DOWN_ARROW_SUFFIX) {
        *reading_arrow = 0;
        if (pthread_mutex_lock(&receiver_data.lock_radio_stations) < 0)
            SIG_SELF;
        if (receiver_data.selected_radio_station != NULL &&
            receiver_data.selected_radio_station->next != NULL) {
            receiver_data.selected_radio_station =
                    receiver_data.selected_radio_station->next;
            if (pthread_mutex_unlock(&receiver_data.lock_radio_stations) < 0)
                SIG_SELF;
            write_list_of_stations_for_all_clients();
        } else if (pthread_mutex_unlock(&receiver_data.lock_radio_stations) < 0)
            SIG_SELF;
    } else
        *reading_arrow = 0;
}

int ui_sock; // Global for signal handler.
void *user_interface(void *arg)
{
    // Prepare to accept connections from clients.
    int i, max_sock, e_sock, select_value;
    struct sockaddr_in server_address, client_address;
    socklen_t client_address_len;
    ui_sock = socket(AF_INET, SOCK_STREAM, 0);
    if (ui_sock < 0)
        SIG_SELF;
    server_address.sin_family = AF_INET; // IPv4
    server_address.sin_addr.s_addr = htonl(INADDR_ANY);
    server_address.sin_port = htons(ui_port);
    if (bind(ui_sock, (struct sockaddr *)&server_address,
             sizeof(server_address)) < 0 ||
        listen(ui_sock, MAX_USERS_CONNECTED) < 0)
        SIG_SELF;

    // Accept either new connections or input from existing ones.
    fd_set readfds;
    ssize_t bytes_read;
    char char_read;
    while (1) {
        FD_ZERO(&readfds);
        FD_SET(ui_sock, &readfds);
        max_sock = ui_sock;
        for (i = 0; i < MAX_USERS_CONNECTED; ++i) {
            e_sock = receiver_data.users_connected[i];
            if (e_sock > 0) {
                FD_SET(e_sock, &readfds);
                if (e_sock > max_sock)
                    max_sock = e_sock;
            }
        }
        select_value = select(max_sock + 1, &readfds, NULL, NULL, NULL);
        if (select_value == -1)
            SIG_SELF;
        if (FD_ISSET(ui_sock, &readfds)) { // New client has arrived.
            i = 0; // Find first empty entry in array to save new client.
            while (i < MAX_USERS_CONNECTED &&
                   receiver_data.users_connected[i] != 0) ++i;
            if (i == MAX_USERS_CONNECTED) continue; // Sorry, no place left :(
            e_sock = accept(ui_sock, (struct sockaddr *)&client_address,
                            &client_address_len);
            if (e_sock > 0) { // If accept fails, we won't terminate everything.
                receiver_data.users_connected[i] = e_sock;
                receiver_data.reading_arrow[i] = 0;
                if (configure_client(e_sock) < 0) {
                    // Something went wrong, terminate connection.
                    close(e_sock);
                    receiver_data.users_connected[i] = 0;
                }
                else
                    write_list_of_stations_for_one_client(e_sock);
            }
        } else { // Input from one of clients.
            for (i = 0; i < MAX_USERS_CONNECTED; ++i) {
                e_sock = receiver_data.users_connected[i];
                if (FD_ISSET(e_sock, &readfds)) {
                    bytes_read = read(e_sock, &char_read, 1);
                    if (bytes_read < 0) {
                        if (errno != EWOULDBLOCK)
                            SIG_SELF;
                    } else if (bytes_read == 0) {
                        // Client terminated connection.
                        close(e_sock);
                        receiver_data.users_connected[i] = 0;
                        receiver_data.reading_arrow[i] = 0;
                    } else
                        process_input_char(char_read,
                                           &receiver_data.reading_arrow[i]);
                }
            }
        }
    }
}

// Functions related to receiving and writing data.

void *send_rexmit(void *arg)
{
    struct rexmit_data_t *data = (struct rexmit_data_t*) arg;
    struct timespec delay;
    delay.tv_sec = retransmission_time / 1000; // seconds
    delay.tv_nsec = (retransmission_time % 1000) * 1000000; // [ms] to [ns]
    struct sockaddr_in dest_addr;
    dest_addr.sin_family = AF_INET;
    dest_addr.sin_port = data->port;
    if (inet_aton(data->mcast_addr, &dest_addr.sin_addr) == 0)
        SIG_SELF;
    while (1) {
        nanosleep(&delay, NULL);
        if (sendto(receiver_data.ctrl_sock, data->msg, strlen(data->msg), 0,
                   (struct sockaddr *)&dest_addr, sizeof(dest_addr))
            < strlen(data->msg))
            SIG_SELF;
    }
}

// Ask for packets [old_byte + 1, old_byte + pack_size], ...,
// [new_byte - pack_size, new_byte - 1].
void ask_for_missing_packets(uint64_t new_byte, uint64_t old_byte,
                             char mcast_addr[ADDRESS_BUFFER_SIZE],
                             uint16_t port)
{
    // Prepare message string.
    size_t __size = data_buffer_size / write_out.pack_size * MAX_UINT64_LEN
                    + strlen(REXMIT_TEXT);
    char *str = malloc(sizeof(char) * __size);
    if (str == NULL)
        SIG_SELF;
    memset(str, 0, __size);
    int i = 0;
    i += sprintf(str, REXMIT_TEXT);
    uint64_t interval_begin = old_byte + 1;
    while (interval_begin < new_byte) {
        i += sprintf(str + i, "%" PRIu64 ",", interval_begin);
        interval_begin += write_out.pack_size;
    }
    str[i - 1] = '\n'; // Remove last unnecessary ','.
    struct rexmit_sender_t *new_sender = malloc(sizeof(struct rexmit_sender_t));
    if (new_sender == NULL) {
        free(str);
        SIG_SELF;
    }
    new_sender->message = str;
    new_sender->next = NULL;

    struct rexmit_data_t *rexmit_data = malloc(sizeof(struct rexmit_data_t));
    rexmit_data->port = port;
    rexmit_data->msg = str;
    memset(rexmit_data->mcast_addr, 0, ADDRESS_BUFFER_SIZE);
    strcpy(rexmit_data->mcast_addr, mcast_addr);
    new_sender->data = rexmit_data;

    pthread_attr_t thread_attr;
    if (pthread_attr_init(&thread_attr) ||
        pthread_create(&new_sender->thread, &thread_attr, send_rexmit,
                       (void *)rexmit_data)) {
        free(str);
        free(new_sender);
        SIG_SELF;
    }
    if (pthread_mutex_lock(&receiver_data.lock_radio_stations) < 0)
        SIG_SELF;
    if (write_out.rexmit_sender_list == NULL) {
        write_out.rexmit_sender_list = new_sender;
        write_out.rexmit_sender_last = new_sender;
    } else {
        write_out.rexmit_sender_last->next = new_sender;
        write_out.rexmit_sender_last = new_sender;
	}
    if (pthread_mutex_unlock(&receiver_data.lock_radio_stations) < 0)
        SIG_SELF;
}

void *receive_data(void *args)
{
    struct sockaddr_in data_addr;
    struct ip_mreq ip_mreq;
    int err, index, floor_bufsize, buffer_filled_size, i;
    struct radio_station_t *data_source;
    ssize_t msg_size;
    size_t MAX_SIZE = sizeof(char) * (data_buffer_size + AUDIO_PACK_INFO_SIZE);
    uint64_t buf_64;
    char sender_mcast_addr[ADDRESS_BUFFER_SIZE];
    uint16_t sender_port;

    // Thread stays infinitely in this loop.
    while (1) {
        memset(sender_mcast_addr, 0, ADDRESS_BUFFER_SIZE);
        // We need radio station to listen to.
        if (pthread_mutex_lock(&receiver_data.lock_radio_stations) < 0)
            SIG_SELF;
        while (receiver_data.selected_radio_station == NULL)
            pthread_cond_wait(&receiver_data.cond_receiver,
                              &receiver_data.lock_radio_stations);
        data_source = receiver_data.selected_radio_station;
        if (write_out.data_sock != 0) { // Clear after previous connection.
            setsockopt(write_out.data_sock, IPPROTO_IP, IP_DROP_MEMBERSHIP,
                       (void*) &ip_mreq, sizeof ip_mreq);
            close(write_out.data_sock);
        }
        free_rexmit_sender(write_out.rexmit_sender_list);
        write_out.rexmit_sender_list = NULL;
        // Achieve connection.
        write_out.data_sock = socket(AF_INET, SOCK_DGRAM, 0);
        if (write_out.data_sock < 0)
            SIG_SELF;
        memset(&data_addr, 0, sizeof(data_addr));
        strcpy(sender_mcast_addr, data_source->mcast_addr);
        sender_port = data_source->port;
        ip_mreq.imr_interface.s_addr = htonl(INADDR_ANY);
        if (inet_aton(sender_mcast_addr, &ip_mreq.imr_multiaddr) == 0 ||
            setsockopt(write_out.data_sock, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                       (void *) &ip_mreq, sizeof ip_mreq) < 0)
            SIG_SELF;
        memset(&data_addr, 0, sizeof(data_addr));
        data_addr.sin_family = AF_INET;
        data_addr.sin_addr.s_addr = htonl(INADDR_ANY);
        data_addr.sin_port = htons(sender_port);
        if (bind(write_out.data_sock, (struct sockaddr *) &data_addr,
                 (socklen_t) sizeof(data_addr)) < 0 ||
            pthread_mutex_unlock(&receiver_data.lock_radio_stations) < 0)
            SIG_SELF;

        // Thread stays in this loop while station doesn't change.
        while (1) {
            // Clear data.
            memset(write_out.buffer, 0, data_buffer_size);
            err = OK;
            write_out.top_byte = 0;
            write_out.filled_bytes = 0;
            // Read first pack (we don't know how big it will be).
            write_out.temp_buffer = realloc(write_out.temp_buffer, MAX_SIZE);
            if (write_out.temp_buffer == NULL)
                SIG_SELF;
            if ((msg_size = read(write_out.data_sock, write_out.temp_buffer,
                                 MAX_SIZE)) < 0)
                    SIG_SELF;
            write_out.session_id = ntohll(*((uint64_t *) write_out.temp_buffer));
            write_out.byte0 = ntohll(*((uint64_t *) (write_out.temp_buffer +
                                                     sizeof(uint64_t))));
            write_out.pack_size = msg_size - AUDIO_PACK_INFO_SIZE;
            write_out.temp_buffer = realloc(write_out.temp_buffer, msg_size);
            if (write_out.temp_buffer == NULL)
                SIG_SELF;
            memcpy(write_out.buffer,
                   write_out.temp_buffer + AUDIO_PACK_INFO_SIZE,
                   write_out.pack_size);
            write_out.filled_bytes = write_out.pack_size;
            write_out.top_byte = write_out.byte0 + write_out.pack_size - 1;
            buffer_filled_size = data_buffer_size / write_out.pack_size;
            floor_bufsize = buffer_filled_size * write_out.pack_size;
            write_out.buffer_filled = realloc(write_out.buffer_filled,
                                              buffer_filled_size *
                                              sizeof(uint64_t));
            if (write_out.buffer_filled == NULL)
                SIG_SELF;
            write_out.buffer_filled[0] = write_out.byte0;
            write_out.top_byte = write_out.byte0 - 1;
            for (i = 1; i < buffer_filled_size; ++i)
                write_out.buffer_filled[i] = 0;
            // Collect data while we have <75% of buffer filled.
            while (4 * write_out.filled_bytes < 3 * data_buffer_size) {
                if (data_source != receiver_data.selected_radio_station) {
                    // Selected radio station has been changed.
                    err = NEW_STATION;
                    break;
                }
                msg_size = read(write_out.data_sock, write_out.temp_buffer,
                                write_out.pack_size + AUDIO_PACK_INFO_SIZE);
                if (msg_size < 0)
                    SIG_SELF;
                if (msg_size < write_out.pack_size + AUDIO_PACK_INFO_SIZE) {
                    continue; // Skip junk.
				}
                // Check session_id.
                buf_64 = ntohll(*((uint64_t *) write_out.temp_buffer));
                if (buf_64 < write_out.session_id)
                    continue; // Skip junk.
                if (buf_64 > write_out.session_id) {
                    err = NEW_SESSION;
                    break;
                }
                // buf64 has first byte number.
                buf_64 = ntohll(*((uint64_t *) (write_out.temp_buffer +
                                                sizeof(uint64_t))));
                index = buf_64 - write_out.byte0;
                if (index > floor_bufsize - write_out.pack_size)
                    index = floor_bufsize - write_out.pack_size;
                memcpy(write_out.buffer + index, write_out.temp_buffer,
                       write_out.pack_size);
                if (write_out.buffer_filled[index / write_out.pack_size] == 0)
                    write_out.filled_bytes += write_out.pack_size; // New data.
                write_out.buffer_filled[index / write_out.pack_size] = buf_64;
                if (buf_64 - write_out.top_byte > write_out.pack_size)
                    ask_for_missing_packets(buf_64, write_out.top_byte,
                                            sender_mcast_addr, sender_port);
                if (buf_64 > write_out.top_byte)
                    write_out.top_byte = buf_64 + write_out.pack_size - 1;
            }
            if (err == NEW_STATION)
                break;
            // Write data to stdout if everything went OK.
            if (err == OK) {
                i = 0;
                while (i < buffer_filled_size) {
                    if (write_out.buffer_filled[i] - write_out.byte0 !=
                        i * write_out.pack_size)
                        break; // Order of data failed.
                    if (write(STDOUT_FILENO,
                              write_out.buffer + i * write_out.pack_size,
                              write_out.pack_size) < write_out.pack_size)
                        SIG_SELF;
                    ++i;
                }
            } // We emptied buffer, keep on listening.
        }
    }
}

// Main and functions related to terminating program.

void input_error()
{
    fprintf(stderr, RECEIVER_USAGE_INFO);
    exit(1);
}

void clean()
{
    int i;
    pthread_mutex_destroy(&receiver_data.lock_radio_stations);
    pthread_cond_destroy(&receiver_data.cond_receiver);
    // Clear radio stations.
    struct radio_station_t *iterator, *helper;
    iterator = receiver_data.first_radio_station;
    while (iterator != NULL) {
        helper = iterator;
        iterator = iterator->next;
        remove_radiostation(helper); // This is more safe than FREE_SAFE.
    }

    FREE_SAFE(write_out.buffer);
    FREE_SAFE(write_out.temp_buffer);
    FREE_SAFE(write_out.buffer_filled);
    free_rexmit_sender(write_out.rexmit_sender_list);
    close(receiver_data.ctrl_sock);
    close(write_out.data_sock);
    close(ui_sock);
    for (i = 0; i < MAX_USERS_CONNECTED; ++i)
        if (receiver_data.users_connected[i] > 0)
            close(receiver_data.users_connected[i]);
}

void handle_signal(int signal)
{
    fprintf(stderr, "Terminating receiver\n");
    if (errno)
        fprintf(stderr, "Error %d: %s\n", errno, strerror(errno));
    clean();
    exit(1); // This exits all threads.
}

int main(int argc, char *argv[])
{
    // Handle input parameters.

    int option;
    unsigned int input_flags = 0, i = 0;
    intmax_t from_strtol;

    while ((option = getopt(argc, argv, "d:C:U:b:R:n:")) != -1) {
        switch (option) {
            case 'd':
                if (input_flags & DISCOVER_ADDR_FLAG) // Option repeated.
                    input_error();
                input_flags |= DISCOVER_ADDR_FLAG;
                if (strlen(optarg) > ADDRESS_BUFFER_SIZE - 1) {
                    fprintf(stderr, "Invalid address\n");
                    exit(1);
                }
                strcpy(discover_address, optarg);
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
            case 'U':
                if (input_flags & UI_PORT_FLAG)
                    input_error();
                input_flags |= UI_PORT_FLAG;
                from_strtol = strtol(optarg, NULL, 10);
                if (errno == ERANGE ||
                    from_strtol <= 0 ||
                    from_strtol > UINT16_MAX) {
                    fprintf(stderr, "Invalid UI port number\n");
                    exit(1);
                }
                ui_port = (uint16_t) from_strtol;
                break;
            case 'b':
                if (input_flags & BUFFER_SIZE_FLAG)
                    input_error();
                input_flags |= BUFFER_SIZE_FLAG;
                from_strtol = strtol(optarg, NULL, 10);
                if (errno == ERANGE ||
                    from_strtol <= 0) {
                    fprintf(stderr, "Invalid data buffer size\n");
                    exit(1);
                }
                data_buffer_size = (int) from_strtol;
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
                retransmission_time = (int) from_strtol;
                break;
            case 'n':
                if (input_flags & SENDER_NAME_FLAG)
                    input_error();
                input_flags |= SENDER_NAME_FLAG;
                strcpy(listento_name, optarg);
                // Since names are stored with '\r\n', append it.
                i = strlen(listento_name);
                listento_name[i] = '\r';
                listento_name[i + 1] = '\n';
                listento_name[i + 2] = 0;
                break;
            default: // Unknown parameter, terminate program.
                input_error();
        }
    }

    // Prepare for receiving signal:
    struct sigaction action;
    action.sa_handler = handle_signal;
    action.sa_flags = 0;
    if (sigaction(SIGTERM, &action, NULL) == -1)
        syserr("sigaction");

    // Achieve connection:
    // Some ports are used only by one thread
    // (user_interface and receive_data). For those, connections are
    // achieved in respecive threads.
    receiver_data.ctrl_sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (receiver_data.ctrl_sock < 0)
        syserr("receiver: socket");
    option = 1; // Enable.
    if (setsockopt(receiver_data.ctrl_sock, SOL_SOCKET, SO_BROADCAST, &option, sizeof(option)) == -1)
        syserr("receiver: setsockopt (SO_BROADCAST)");
    option = 5; // Time to live.
    if (setsockopt(receiver_data.ctrl_sock, IPPROTO_IP, IP_MULTICAST_TTL, &option, sizeof(option)) < 0)
        syserr("sender: setsockopt (IP_MULTICAST_TTL)");
    ctrl_recv_addr.sin_family = AF_INET;
    ctrl_recv_addr.sin_port = htons(control_port);
    ctrl_recv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    if (bind(receiver_data.ctrl_sock, (struct sockaddr *)&ctrl_recv_addr, sizeof ctrl_recv_addr) < 0)
        syserr("receiver: bind");
    ctrl_send_addr.sin_family = AF_INET;
    ctrl_send_addr.sin_port = htons(control_port);
    if (inet_aton(discover_address, &ctrl_send_addr.sin_addr) == 0)
        syserr("receiver: inet_aton");

    // Allocate buffers:
    write_out.buffer = malloc(sizeof(char) * data_buffer_size);
    if (write_out.buffer == NULL)
        syserr("malloc");
    memset(write_out.buffer, 0, data_buffer_size);

    // Clear global variables:
    for (i = 0; i < MAX_USERS_CONNECTED; ++i) {
        receiver_data.users_connected[i] = 0;
        receiver_data.reading_arrow[i] = 0;
    }
    receiver_data.first_radio_station = NULL;
    receiver_data.selected_radio_station = NULL;
    write_out.buffer_filled = NULL;
    write_out.temp_buffer = NULL;
    write_out.rexmit_sender_list = NULL;
    write_out.rexmit_sender_last = NULL;
    write_out.pack_size = 0;
    write_out.top_byte = 0;
    write_out.filled_bytes = 0;
    write_out.byte0 = 0;
    write_out.session_id = 0;
    write_out.data_sock = 0;

    // Prepare and start threads:
    if (pthread_mutex_init(&receiver_data.lock_radio_stations, 0)) {
        free(write_out.buffer);
        syserr("pthread_mutex_init");
    }
    pthread_attr_t thread_attr;
    if (pthread_attr_init(&thread_attr) ||
        pthread_cond_init(&receiver_data.cond_receiver, NULL) ||
        pthread_create(&discover_thread, &thread_attr, discover_radiostations, 0) ||
        pthread_create(&lookups_thread, &thread_attr, receive_lookups, 0) ||
        pthread_create(&clean_thread, &thread_attr, remove_inactives, 0) ||
        pthread_create(&data_thread, &thread_attr, receive_data, 0) ||
        pthread_create(&ui_thread, &thread_attr, user_interface, 0)) {
        handle_signal(0);
    }

    void *return_value;
    // Endless loop: wait for any thread other than data thread.
    pthread_join(ui_thread, &return_value);
    // We shouldn't be there - terminate receiver.
    handle_signal(0);
}
