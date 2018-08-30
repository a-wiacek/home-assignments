#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <sys/fcntl.h>
#include <mqueue.h>
#include <memory.h>
#include <errno.h>
#include "helper.h"
#include "err.h"

void close_all(int signal)
{
    fprintf(stderr, "Terminating run\n");
    if (errno) {
        fprintf(stderr, "Error %d: %s\n", errno, strerror(errno));
    }
    system("pkill -f validator -SIGTERM");
    exit(-1);
}

void send_result(char *destination, char *answer, char *word, char *pid)
{
    int dest = atoi(destination);
    char buffer[EXTLEN];
    if (dest == -1) {
        if (answer[0] == 'A')
            sprintf(buffer, "%s %s A", pid, word);
        else // this includes "weak no"
            sprintf(buffer, "%s %s N", pid, word);
        mqd_t results_queue = mq_open(RESULTS, O_WRONLY);
        if (results_queue == (mqd_t) -1) {
            ERROR2("run", "send_result->mq_open(RESULTS, ...)");
        }
        if (mq_send(results_queue, buffer, strlen(buffer), 0)) {
            mq_close(results_queue);
            ERROR2("run", "send_result->mq_send(RESULTS, ...)");
        }
        if (mq_close(results_queue)) {
            ERROR2("run", "send_result->mq_close(RESULTS, ...)");
        }
    } else {
        if (write(dest, answer, 1) != 1) {
            ERROR2("run", "send_result->write(RESULTS, ...)");
        }
    }
    exit(0);
}

int get_amount_of_states(const char *parameters)
{
    int i = 0, answer = 0;
    while (1) {
        while (parameters[i] >= '0' && parameters[i] <= '9') ++i;
        ++answer;
        if (parameters[i] == '\0' || parameters[i] == '\n') {
            return answer;
        }
        ++i;
    }
}

// Result is saved in t1
void char_or(char* t1, const char* t2)
{
    if (t1[0] == 'A' || t2[0] == 'A') t1[0] = 'A';
    else if (t1[0] == 'N' || t2[0] == 'N') t1[0] = 'N';
    else t1[0] = 'W';
}
void char_and(char* t1, const char* t2)
{
    if (t1[0] == 'N' || t2[0] == 'N') t1[0] = 'N';
    else if (t1[0] == 'A' || t2[0] == 'A') t1[0] = 'A';
    else t1[0] = 'W';
}

// Parameters in argv:
// argv[0] - run
// argv[1] - pid of tester process that requested word
// argv[2] - file descriptor to send result (if -1, send to queue RESULTS)
// argv[3] - initial state
// argv[4] - size of automata description
// argv[5] - file descriptor to receive automata description from
// argv[6] - word (empty word is represented by "\n")

int main(int argc, char **argv)
{
    if (argc != 7) {
        fprintf(stderr, "ERROR in run (pid: %d)\n    Incorrect parameters\n",
                getpid());
        close_all(errno);
    }
    
    // Prepare for closing
    struct sigaction action;
    action.sa_handler = close_all;
    action.sa_flags = 0;
    if (sigaction(SIGTERM, &action, NULL) == -1) {
        ERROR("sigaction");
    };
    
    char buffer[EXTLEN] = {0}, buffer2[EXTLEN] = {0},
         simple_buffer1[8] = {0}, simple_buffer2[8] = {0},
         simple_buffer3[8] = {0};

    // Get automata from pipe, close pipe
    int automata_size = atoi(argv[4]);
    char automata_description[automata_size][EXTLEN];
    if (read(atoi(argv[5]), automata_description, automata_size * EXTLEN) == -1) {
        ERROR("read");
    }
    if (close(atoi(argv[5]))) {
        ERROR(close);
    }

    if (argv[6][0] == '\n') { // Easy case: empty word
        int state = atoi(argv[3]), curr_state, i = 0;
        if (automata_description[1][0] == '-') {
            SEND_RESULT("N");
        }
        while (1) {
            curr_state = 0;
            while (automata_description[1][i] >= '0' &&
                   automata_description[1][i] <= '9') {
                curr_state = 10 * curr_state + automata_description[1][i] - '0';
                ++i;
            }
            if (curr_state == state) {
                SEND_RESULT("A");
            }
            if (automata_description[1][i] == '\0' ||
                automata_description[1][i] == '\n') {
                SEND_RESULT("N");
            }
            ++i;
        }
    } else { // Second case: non-empty word
        char key[20];
        sprintf(key, "%s %c", argv[3], argv[6][0]);
        // item contains line T(q,a) (if it exists)
        char *item = (char *) bsearch(key, automata_description + 2,
                                      automata_size - 2, sizeof(char) * EXTLEN,
                                      my_compare);
        if (item == NULL) { // README 2.7
            SEND_RESULT("W");
        } else { // Divide work
            int index = 0, universal_states = atoi(automata_description[0]),
                state = atoi(argv[3]);
            for (int i = 0; i < 2; ++i) {
                while (*item != ' ') ++item;
                ++item;
            }
            // Now index is pointing at the first state from set T(q,a)
            int tab_size = get_amount_of_states(item);
            int fds[tab_size][4];

            // Open pipes and exec runs
            // We need to create two pipes for each run:
            // - to pass automata description
            // - to receive result
            for (int i = 0; i < tab_size; ++i) {
                if (pipe(fds[i]) || pipe(fds[i] + 2)) {
                    ERROR("pipe");
                }
                if (i) {
                    while (item[index] != ' ') ++index;
                    ++index;
                }
                switch (fork()) {
                    case -1:
                        ERROR("fork");

                    case 0: // child
                        if (close(fds[i][0]) || close(fds[i][3])) {
                            ERROR("close");
                        }
                        sprintf(simple_buffer1, "%d", fds[i][1]);
                        sprintf(simple_buffer2, "%d", atoi(item + index));
                        sprintf(simple_buffer3, "%d", fds[i][2]);
                        if (argv[6][1] == '\0') {
                            execl("./run", "run", argv[1], simple_buffer1,
                                  simple_buffer2, argv[4], simple_buffer3,
                                  "\n", NULL);
                        } else {
                            execl("./run", "run", argv[1], simple_buffer1,
                                  simple_buffer2, argv[4], simple_buffer3,
                                  argv[6] + 1, NULL);
                        }
                        ERROR("execl");

                    default: // parent
                        if (close(fds[i][1]) || close(fds[i][2])) {
                            ERROR("close");
                        }
                        // pass automata through pipe
                        if (write(fds[i][3], (char *)automata_description,
                                  sizeof(char) * automata_size * EXTLEN) < 0) {
                            ERROR("write");
                        }
                        if (close(fds[i][3]) == -1) {
                            ERROR("close");
                        }
                        break;
                }
            }

            // Collect data from workers
            if (read(fds[0][0], buffer2, EXTLEN) == -1) {
                ERROR("read");
            }
            if (close(fds[0][0]) == -1) {
                ERROR("close");
            }
            for (int i = 1; i < tab_size; ++i) {
                if (read(fds[i][0], buffer, EXTLEN) == -1) {
                    ERROR("read");
                }
                if (state < universal_states) char_and(buffer2, buffer);
                else char_or(buffer2, buffer);
                if (close(fds[i][0]) == -1) {
                    ERROR("close");
                }
            }
            for (int i = 0; i < tab_size; ++i) {
                if (wait(0) == -1) {
                    ERROR("wait");
                }
            }
            if (buffer2[0] == 'A') {
                SEND_RESULT("A");
            } else {
                SEND_RESULT("N");
            }
        }
    }
}
