#ifndef ANNOUNCE_MESSAGE_H
#define ANNOUNCE_MESSAGE_H

#include "ma.h"
#include "chat.h"

const size_t MSG_MAX_LEN = 1024;

struct
    __attribute__ ((packed))
    packed_message
{
    chat_time_t timestamp;
    _mac_addr_t ma;
};

struct announce_message
{
    void to_net();
    void to_host();
    void update();

    announce_message();
    announce_message(const packed_message &);

    mac_addr_t mac_addr;
    chat_time_t timestamp;
    int ip;
};

void copy_packed_message(packed_message &, const announce_message &);
void copy_announce_message(announce_message &, const packed_message &);


#endif
