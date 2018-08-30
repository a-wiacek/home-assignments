#ifndef SENDER_H
#define SENDER_H

const char *SENDER_USAGE_INFO = ""
 "Use with obligatory parameter -a: send address (dots notation)\n"
 "List of optional parameters:\n"
 "-P: data port (default: 26349)\n"
 "-C: control port (default: 36349)\n"
 "-p: pack size [B] (default: 512)\n"
 "-f: FIFO queue size [B] (default: 131072)\n"
 "-R: retransmission time [ms] (default: 250)\n"
 "-n: name (default: Nienazwany Nadajnik)\n";

#define INITIAL_FIFO_SIZE 131072 // 128kB
#define INITIAL_SENDER_NAME "Nienazwany Nadajnik"

#endif // SENDER_H
