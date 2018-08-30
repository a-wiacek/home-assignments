#ifndef CONST_H
#define CONST_H

#include <stdint.h>

// ---------------------------------------- //
// Constants common for sender and receiver //
// ---------------------------------------- //

#define INDEX_NR_MOD 6349 // 386349
#define INITIAL_DATA_PORT (20000 + INDEX_NR_MOD)
#define INITIAL_CONTROL_PORT (30000 + INDEX_NR_MOD)
#define INITIAL_RETRANSMISSION_TIME 250
#define ADDRESS_BUFFER_SIZE 20
#define SENDER_NAME_MAX_SIZE 69
#define TEMP_BUFFER_SIZE 1500
#define INITIAL_PACK_SIZE 512

const char *LOOKUP_TEXT = "ZERO_SEVEN_COME_IN\n";
const char *REPLY_TEXT = "BOREWICZ_HERE ";
const char *REXMIT_TEXT = "LOUDER_PLEASE ";

#define SIG_SELF raise(SIGTERM)

struct audio_pack {
    uint64_t session_id;
    uint64_t first_byte_num;
    char *audio_data;
};
#define AUDIO_PACK_INFO_SIZE (sizeof(struct audio_pack) - sizeof(char*))

// Hton and ntoh functions for 64bit integets
// https://stackoverflow.com/questions/3022552/is-there-any-standard-htonl-like-function-for-64-bits-integers-in-c
#define htonll(x) ((1==htonl(1)) ? (x) : ((uint64_t)htonl((x) & 0xFFFFFFFF) << 32) | htonl((x) >> 32))
#define ntohll(x) ((1==ntohl(1)) ? (x) : ((uint64_t)ntohl((x) & 0xFFFFFFFF) << 32) | ntohl((x) >> 32))

#define FREE_SAFE(ptr) if ((ptr) != NULL) free(ptr);

// Flags used to check validity of input:
// - was mandatory -a in sender passed?
// - was any argument passed twice?

#define MCAST_ADDR_FLAG 0x001
#define DISCOVER_ADDR_FLAG 0x002
#define DATA_PORT_FLAG 0x004
#define CONTROL_PORT_FLAG 0x008
#define UI_PORT_FLAG 0x010
#define PACK_SIZE_FLAG 0x020
#define BUFFER_SIZE_FLAG 0x040
#define FIFO_SIZE_FLAG 0x080
#define RETRANSMISSION_TIME_FLAG 0x100
#define SENDER_NAME_FLAG 0x200

#endif // CONST_H
