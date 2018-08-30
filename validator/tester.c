#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <mqueue.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <string.h>
#include <sys/param.h>
#include <stdlib.h>
#include <errno.h>
#include "helper.h"
#include "err.h"

mqd_t requests_queue, answers_queue;
char ANSWERS_PID[20];

void close_all(int signal)
{
    fprintf(stderr, "Terminating tester %d\n", getpid());
    if (errno) {
        fprintf(stderr, "Error %d: %s\n", errno, strerror(errno));
    }
    mq_close(requests_queue);
    mq_close(answers_queue);
    mq_unlink(ANSWERS_PID);
    system("pkill -f validator -SIGTERM");
    exit(-1);
}

// Since read function reads more than one line at time (it fills buffer),
// we need to extract one line of text into separate buffer
// Return values:
// 0 - read buffer doesn't contain whole line so we can't process this line
// 1 - read buffer contains valid line

int extract_line(const char *read_buffer, char *write_buffer,
                 int *read_index, int *write_index)
{
    if (*write_index == 0) {
        memset(write_buffer, 0, EXTLEN);
    }
    while (1) {
        if (*read_index == MAXLEN || read_buffer[*read_index] == '\0') {
            *read_index = 0;
            return 0;
        }
        if (read_buffer[*read_index] == '\n') {
            write_buffer[*write_index] = read_buffer[*read_index];
            ++(*write_index);
            ++(*read_index);
            write_buffer[*write_index] = '\0';
            *write_index = 0;
            return 1;
        }
        write_buffer[*write_index] = read_buffer[*read_index];
        ++(*write_index);
        ++(*read_index);
    }
}

int main(int argc, char **argv)
{
    int sent = 0, received = 0, accepted = 0, fake_received = 0,
        select_value, read_bytes, stdin_open = 1, nfds,
        read_index = 0, write_index = 0, validator_working = 1;
    // extra space is for pid and answer
    char buffer[EXTLEN] = {0}, prepared_message[EXTLEN] = {0},
         stdin_buffer[EXTLEN] = {0};
    printf("PID: %d\n", getpid());

    // Prepare for receiving signal
    struct sigaction action;
    action.sa_handler = close_all;
    action.sa_flags = 0;
    if (sigaction(SIGTERM, &action, NULL) == -1) {
        syserr("sigaction"); // Error here won't terminate all processes
    };
    /* For internal debugging
    if (sigaction(SIGINT, &action, NULL) == -1) {
        syserr("sigaction"); // Error here won't terminate all processes
    };
    */
    struct mq_attr mq_a;
    mq_a.mq_maxmsg = 10;
    mq_a.mq_msgsize = EXTLEN;

    // This queue is already opened by validator
    requests_queue = mq_open(REQUESTS, O_WRONLY, 0777, &mq_a);
    if (requests_queue == (mqd_t) -1) {
        ERROR("mq_open(REQUESTS, ...)");
    }

    // Preparing name of second queue
    sprintf(ANSWERS_PID, "%s%d", ANSWERS, getpid());

    // This queue is used to receive answers from validator
    answers_queue = mq_open(ANSWERS_PID, O_RDONLY | O_CREAT, 0777, &mq_a);
    if (answers_queue == (mqd_t) -1) {
        mq_close(requests_queue);
        ERROR("mq_open(ANSWERS_PID, ...)");
    }

    fd_set readfds;

    // First loop which reads words, passes them to validator
    // and receives answers from validator.
    // Set up two sources of data: stdin and ANSWERS queue
    // It works until stdin reads EOF,
    // tester receives info that validator has finished accepting new words or
    // tester sends message "!" (which signals validator to stop working)
    while (1) {
        FD_ZERO(&readfds);
        FD_SET(answers_queue, &readfds);
        FD_SET(STDIN_FILENO, &readfds);
        nfds = MAX(answers_queue, STDIN_FILENO);
        memset(buffer, 0, EXTLEN);
        select_value = select(nfds + 1, &readfds, NULL, NULL, NULL);
        if (select_value == -1) {
            ERROR("select");
        }
        // Note: between moments where validator decides to stop accepting new
        // words and tester receives info about that we may send him a few more
        // words. Validator will "ignore" those requests - send back "?" signal
        // (unless validator has stopped working).
        // We count these as "fake received" - we need to count them for
        // purpose of managing message queue, but they don't count as received.
        if (FD_ISSET(answers_queue, &readfds)) {
            read_bytes = mq_receive(answers_queue, buffer, EXTLEN, NULL);
            if (read_bytes == -1) { // Error while reading
                ERROR("mq_receive(ANSWERS_PID, ...)");
            }
            if (buffer[0] == '!') { // Validator won't accept new words
                validator_working = 0;
                break;
            } else if (buffer[0] == '?') {
                // ASSUMPTION
                ++fake_received;
                break;
            } else { // Result of request we sent
                printf("%s\n", buffer);
                // Update local variables
                ++received;
                if (buffer[strlen(buffer) - 1] == 'A') {
                    ++accepted;
                }
            }
        } else if (FD_ISSET(STDIN_FILENO, &readfds)) {
            memset(stdin_buffer, 0, EXTLEN);
            read_bytes = read(STDIN_FILENO, stdin_buffer, MAXLEN);
            if (read_bytes == -1) { // Error while reading
                ERROR("read(STDIN, ...)");
            }
            if (read_bytes == 0) { // End of file
                stdin_open = 0;
                break;
            }
            int exclamation_sign = 0;
            while (1) {
                if (!extract_line(stdin_buffer, buffer,
                                  &read_index, &write_index)) {
                    break;
                }
                // we need to remember this index only when we have part of line
                // (we will get second part from next read)
                write_index = 0;
                if (buffer[0] == '!') { // We send info to stop validator
                    // Note: we don't increment sent here
                    sprintf(prepared_message, "! %d", getpid());
                    if (mq_send(requests_queue, prepared_message, 12, 0)) {
                        ERROR("mq_send(REQUESTS, ...)");
                    }
                    validator_working = 0;
                    exclamation_sign = 1;
                    break;
                } else { // Just an ordinary request
                    memset(prepared_message, 0, EXTLEN);
                    sprintf(prepared_message, "%d %s", getpid(), buffer);
                    if (mq_send(requests_queue, prepared_message,
                                strlen(prepared_message), 0)) {
                        ERROR("mq_send(REQUESTS, ...)");
                    }
                    ++sent;
                }
            }
            if (exclamation_sign) {
                break;
            }
        }
    }

    while (sent > received + fake_received || stdin_open) {
        FD_ZERO(&readfds);
        FD_SET(answers_queue, &readfds);
        if (stdin_open) {
            FD_SET(STDIN_FILENO, &readfds);
            nfds = MAX(answers_queue, STDIN_FILENO);
        } else {
            nfds = answers_queue;
        }
        memset(buffer, 0, EXTLEN);
        select_value = select(nfds + 1, &readfds, NULL, NULL, NULL);
        if (select_value == -1) {
            ERROR("select");
        }
        if (FD_ISSET(answers_queue, &readfds)) {
            read_bytes = mq_receive(answers_queue, buffer, EXTLEN, NULL);
            if (read_bytes == -1) { // Error while reading
                ERROR("mq_receive(ANSWERS_PID, ...)");
            } else if (buffer[0] == '!') {
                continue;
            } else if (buffer[0] == '?') {
                // ASSUMPTION
                ++fake_received;
            } else { // Result of request we sent
                printf("%s\n", buffer);
                // Update local variables
                ++received;
                if (buffer[strlen(buffer) - 1] == 'A') {
                    ++accepted;
                }
            }
        } else if (FD_ISSET(STDIN_FILENO, &readfds)) {
            read_bytes = read(STDIN_FILENO, buffer, MAXLEN);
            if (read_bytes < 0) { // Error while reading
                ERROR("read(STDIN, ...)");
            } else if (read_bytes == 0) { // End of file
                stdin_open = 0;
                continue;
            } // We don't care what is in message
        }
    }

    // END - release all memory and queues, write raport
    // Here we don't really care about errors
    if (validator_working) {
        sprintf(prepared_message, ". %d", getpid());
        mq_send(requests_queue, prepared_message, strlen(prepared_message), 1);
    }
    mq_close(requests_queue);
    mq_close(answers_queue);
    mq_unlink(ANSWERS_PID);
    printf("Snt: %d\nRcd: %d\nAcc: %d\n", sent, received, accepted);
}
