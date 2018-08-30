#ifndef RECEIVER_H
#define RECEIVER_H

#include <string.h>
#include <unistd.h>

const char *RECEIVER_USAGE_INFO = ""
 "List of optional parameters:\n"
 "-d: discover address (default: 255.255.255.255)\n"
 "-P: data port (default: 26349)\n"
 "-C: control port (default: 36349)\n"
 "-U: user interface port (default: 16349)\n"
 "-b: buffer size [B] (default: 65536)\n"
 "-R: retransmission time [ms] (default: 250)\n"
 "-n: preferred radio station name (no default value)\n";

#define INITIAL_DISCOVER_ADDRESS "255.255.255.255"
#define INITIAL_DATA_BUFFER_SIZE 65536 // 64kB
#define INITIAL_UI_PORT (10000 + INDEX_NR_MOD)
#define MAX_USERS_CONNECTED 200 // My assumption.
#define LOOKUP_DELAY 5 // [s]
#define MAX_INACTIVE_TIME 20 // [s]
#define MAX_UINT64_LEN 20

// Constants in data thread
#define OK 0
#define NEW_SESSION 1
#define NEW_STATION 2

// User interface constants

const char *CLEAR_SCREEN = "\033[H\033[J";
// https://stackoverflow.com/questions/273261/force-telnet-client-into-character-mode
const char *TELNET_CONF = "\377\375\042\377\373\001";

int clear_screen(int sock)
{
    if (write(sock, CLEAR_SCREEN, strlen(CLEAR_SCREEN)) < strlen(CLEAR_SCREEN))
        return -1;
    return 0;
}

int configure_client(int sock)
{
    if (write(sock, TELNET_CONF, strlen(TELNET_CONF)) < strlen(TELNET_CONF))
        return -1;
    return 0;
}

const char ARROW_PREFIX[] = {27, 91};
const char UP_ARROW_SUFFIX = 'A';
const char DOWN_ARROW_SUFFIX = 'B';

const char *RADIO_HEADER = ""
 "------------------------------------------------------------------------\r\n"
 "  SIK Radio\r\n"
 "------------------------------------------------------------------------\r\n";
const char *NOTHING_UP = "Brak dostepnych stacji\r\n";
const char *UI_PTR = "  > ";
const char *UI_NO_PTR = "    ";
const char *END_LINE = ""
 "------------------------------------------------------------------------\r\n";

#endif // RECEIVER_H
