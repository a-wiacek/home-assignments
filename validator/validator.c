#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <mqueue.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <stdlib.h>
#include <errno.h>
#include "helper.h"
#include "err.h"

mqd_t results_queue, requests_queue;

// We will open queue to send answers to tester
// after receiving results from run
// and close it right after sending answer.
// We will store data about testers' requests in those two arrays
int received[PID_MAX] = {0}, accepted[PID_MAX] = {0};
// This array is to know if process tester is running at all
// (we don't need to send message "!" to testers which finished working).
int tester_working[PID_MAX] = {0};

void close_all(int signal)
{
    fprintf(stderr, "Terminating validator\n");
    if (errno) {
        fprintf(stderr, "Error %d: %s\n", errno, strerror(errno));
    }
    mq_close(requests_queue);
    mq_close(results_queue);
    mq_unlink(REQUESTS);
    mq_unlink(RESULTS);
    kill(0, SIGKILL); // kill all children
    for (int i = 0; i < PID_MAX; ++i) {
        if (tester_working[i]) {
            kill(i, SIGKILL);
        }
    }
    exit(-1);
}

void send_to_tester(int pid, const char *msg, size_t msg_size, unsigned priority)
{
    char ANSWERS_PID[20];
    sprintf(ANSWERS_PID, "%s%d", ANSWERS, pid);
    if (msg[0] == '!') {
		mqd_t answers_queue = mq_open(ANSWERS_PID, O_WRONLY);
        if (answers_queue == (mqd_t) -1) {
            ERROR2("validator", "send_to_tester->mq_open(ANSWERS_PID, ...)");
        }
        if (mq_send(answers_queue, msg, msg_size, priority)) {
            ERROR2("validator", "send_to_tester->mq_send(ANSWERS_PID, ...)");
        }
        if (mq_close(answers_queue)) {
            ERROR2("validator", "send_to_tester->mq_close(ANSWERS_PID)");
        }
        return;
    }
    // remove '\n' from message
    char ch_msg[msg_size + 2];
    int i = 0, j = 0;
    do {
        if (msg[i] == '\n') ++i;
        else {
            ch_msg[j] = msg[i];
            ++i; ++j;
        }
    } while (msg[i - 1] != 'A' && msg[i - 1] != 'N');
    ch_msg[j] = '\0'; ++j;
    mqd_t answers_queue = mq_open(ANSWERS_PID, O_WRONLY);
    if (answers_queue == (mqd_t) -1) {
        ERROR2("validator", "send_to_tester->mq_open(ANSWERS_PID, ...)");
    }
    if (mq_send(answers_queue, ch_msg, j, priority)) {
        ERROR2("validator", "send_to_tester->mq_send(ANSWERS_PID, ...)");
    }
    if (mq_close(answers_queue)) {
        ERROR2("validator", "send_to_tester->mq_close(ANSWERS_PID)");
    }
}

int main(int argc, char **argv)
{
    // Validator needs only to know exactly input_size and
    // whether accepting_states_size > 0
    int working = 1, tasks = 0,
        total_received = 0, total_sent = 0, total_accepted = 0,
        nfds, select_value, read_bytes, tester_pid, i;
    char buffer[EXTLEN] = {0};

    // Prepare for receiving signal
    struct sigaction action;
    action.sa_handler = close_all;
    action.sa_flags = 0;
    if (sigaction(SIGTERM, &action, NULL) == -1) {
        syserr("sigaction"); // Error here won't terminate all processes
    };

    struct mq_attr mq_a;
    mq_a.mq_maxmsg = 10;
    mq_a.mq_msgsize = EXTLEN;

    // Create queue to receive results from run
    results_queue = mq_open(RESULTS, O_RDONLY | O_CREAT, 0777, &mq_a);
    if (results_queue == (mqd_t) -1) {
        ERROR("mq_open(RESULTS, ...)");
    }

    // Create queue to receive requests from all testers
    requests_queue = mq_open(REQUESTS, O_RDONLY | O_CREAT, 0777, &mq_a);
    if (requests_queue == (mqd_t) -1) {
        ERROR("mq_open(REQUESTS, ...)");
    }

    // ------------------------- Reading input ---------------------------------

    // Custom automata structure:
    // First line: U
    // Second line: accepting states ("-" if none)
    // Next lines: automata description

    // First six numbers (read in first two lines):
    // (N)umber or lines,       - necessary to interpret automata_description
    // (A)lphabet size,         - we ignore this because we assume valid input
    // (Q)uantity of states,    -                   - || -
    // (U)niversal states,
    // (F)Quantity of accepting states,
    // (q)Initial state

    // First line
    int input_size, universal_states, acc_states;
    char initial_state[8], automata_size_str[8];
    scanf("%d %*d %*d %d %d\n", &input_size, &universal_states, &acc_states);

    sprintf(automata_size_str, "%d", AUTOMATA_SIZE);
    char automata_description[AUTOMATA_SIZE][EXTLEN];
    sprintf(automata_description[0], "%d", universal_states);

    // Second line - this will be passed as argument in run, so
    // we save that outside of huge array
    if (fgets(initial_state, 8, stdin) == NULL) {
        ERROR("fgets(...)");
    }
    initial_state[strcspn(initial_state, "\n")] = 0;

    // Third line - if acc_states = 0, we add "-" as 2nd line of array
    if (acc_states) {
        if (fgets(automata_description[1], EXTLEN, stdin) == NULL) {
            ERROR("fgets(...)");
        }
        automata_description[1][strcspn(automata_description[1], "\n")] = 0;
    } else {
        sprintf(automata_description[1], "-");
    }
    // Next lines - function description
    for (i = 2; i < AUTOMATA_SIZE; ++i) {
        if (fgets(automata_description[i], EXTLEN, stdin) == NULL) {
            ERROR("fgets(...)");
        }
        automata_description[i][strcspn(automata_description[i], "\n")] = 0;
    }

    // Sort function description so run processes can binsearch later on
    qsort(automata_description + 2, AUTOMATA_SIZE - 2, sizeof(char) * EXTLEN,
          my_compare);

    // ---------------------- End of reading input -----------------------------

    // Set up two sources of data: RESULTS and REQUESTS queue
    fd_set readfds;

    // Main loop
    while (working || tasks) {
        FD_ZERO(&readfds);
        FD_SET(results_queue, &readfds);
        FD_SET(requests_queue, &readfds);
        nfds = MAX(results_queue, requests_queue);
        memset(buffer, 0, EXTLEN);
        select_value = select(nfds + 1, &readfds, NULL, NULL, NULL);
        if (select_value == -1) {
            ERROR("select");
        }
        if (FD_ISSET(requests_queue, &readfds)) {
            read_bytes = mq_receive(requests_queue, buffer, EXTLEN, NULL);
            if (read_bytes < 0) {
                ERROR("mq_receive(REQUESTS, ...)");
            }
            if (buffer[0] == '.') {
                tester_working[atoi(buffer + 2)] = 0;
            } else if (working && buffer[0] == '!') {
                working = 0;
                for (i = 0; i < PID_MAX; ++i) {
                    if (tester_working[i]) {
                        send_to_tester(i, "!", 1, 1);
                    }
                }
                tester_working[atoi(buffer + 2)] = 1;
            } else if (working) { // Ordinary request
                // Child: exec run, open pipe
                // Parent: pass data to pipe
                int pipes[2];
                char pipe_str[10];
                if (pipe(pipes) == -1) {
                    ERROR("pipe");
                }
                switch (fork()) {
                    case -1:
                        ERROR("fork");

                    case 0: // child
                        if (close(pipes[1]) == -1) {
                            ERROR("close");
                        }
                        sprintf(pipe_str, "%d", pipes[0]); // Close pipe later!
                        i = 0, tester_pid = 0;
                        while (buffer[i] != ' ') {
                            tester_pid = 10 * tester_pid + buffer[i] - '0';
                            ++i;
                        }
                        ++i;
                        char text_pid[10];
                        sprintf(text_pid, "%d", tester_pid);
                        execl("./run", "run", text_pid, "-1", initial_state,
                              automata_size_str, pipe_str, buffer + i, NULL);
                        ERROR("execl");

                    default: // parent
                        ++tasks;
                        tester_pid = atoi(buffer);
                        ++received[tester_pid];
                        ++total_received;
                        tester_working[tester_pid] = 1;
                        if (close(pipes[0]) == -1) {
                            ERROR("close");
                        }
                        if (write(pipes[1], (char *)automata_description,
                                  sizeof(char) * AUTOMATA_SIZE * EXTLEN) < 0) {
                            ERROR("write");
                        }
                        if (close(pipes[1]) == -1) {
                            ERROR("close");
                        }

                }
            } else { // Validator won't calculate new tasks
                i = 0, tester_pid = 0;
                // Get pid from string
                while (buffer[i] != ' ') {
                    tester_pid = 10 * tester_pid + buffer[i] - '0';
                    ++i;
                }
                send_to_tester(tester_pid, "?", 1, 0);
            }
        } else if (FD_ISSET(results_queue, &readfds)) {
            read_bytes = mq_receive(results_queue, buffer, EXTLEN, NULL);
            if (wait(0) == -1) {
                ERROR("wait");
            };
            if (read_bytes < 0) {
                ERROR("mq_receive(RESULTS, ...)");
            } else { // Ordinary answer
                // Skip to char after first space and get pid
                i = 0, tester_pid = 0;
                while (buffer[i] != ' ') {
                    tester_pid = 10 * tester_pid + buffer[i] - '0';
                    ++i;
                }
                ++i;
                send_to_tester(tester_pid, buffer + i, strlen(buffer) - i, 0);
                // Update local variables
                // Skip to char after second space and get A|N
                --tasks;
                while (buffer[i] != ' ') ++i;
                ++i;
                ++total_sent;
                if (buffer[i] == 'A') {
                    ++accepted[tester_pid];
                    ++total_accepted;
                }
            }
        }
    }

    // END - release all memory and queues, write raport
    // Here we don't really care about errors
    mq_close(requests_queue);
    mq_close(results_queue);
    mq_unlink(REQUESTS);
    mq_unlink(RESULTS);
    printf("Snt: %d\nRcd: %d\nAcc: %d\n", total_sent, total_received,
                                          total_accepted);
    for (i = 0; i < PID_MAX; ++i) {
        if (received[i] > 0)
            printf("PID: %d\nRcd: %d\nAcc: %d\n", i, received[i], accepted[i]);
    }
    return 0;
}
