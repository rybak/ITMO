#ifndef MESSAGE_H
#define MESSAGE_H

typedef unsigned char[6] mac_addr_t;

struct announce_message
{
    long long timestamp;
    unsigned char mac_addr[6];
};


#endif
