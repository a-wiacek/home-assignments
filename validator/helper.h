#ifndef HELPER_H
#define HELPER_H

#define MAXLEN 1000
#define EXTLEN (MAXLEN + 15)

// Maximum ID of process
// Assumption: value is taken from students
// cat /proc/sys/kernel/pid_max
#define PID_MAX 49152
#define AUTOMATA_SIZE (input_size + (acc_states ? -1 : 0))

const char *REQUESTS = "/requests";
const char *RESULTS = "/results";
const char *ANSWERS = "/answers";

#define SEND_RESULT(r) \
do { \
    send_result(argv[2], r, argv[6], argv[1]); \
    return 0; \
} while (0)

int my_compare(const void* a, const void* b)
{
    if (atoi((char *)a) < atoi((char *)b)) return -1;
    if (atoi((char *)a) > atoi((char *)b)) return 1;
    int i = 0, j = 0;
    while (((char *)a)[i] != ' ') ++i; ++i;
    while (((char *)b)[j] != ' ') ++j; ++j;
    if (((char *)a)[i] < ((char *)b)[j]) return -1;
    if (((char *)a)[i] > ((char *)b)[j]) return 1;
    return 0;
}

#define ERROR(fn) \
do { \
    fprintf(stderr, "ERROR in %s (pid: %d)\n    Function %s\n", \
            argv[0], getpid(), fn); \
    close_all(errno); \
} while (0)

#define ERROR2(name, fn) \
do { \
    fprintf(stderr, "ERROR in %s (pid: %d)\n    Function %s\n", \
            name, getpid(), fn); \
    close_all(errno); \
} while (0)

#endif // HELPER_H
