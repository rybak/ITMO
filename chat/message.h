#ifndef MESSAGE_H
#define MESSAGE_H

#include "ma.h"

const size_t MSG_MAX_LEN = 1024;

struct announce_message
{
    void to_net();
    void to_host();
    void update();
    announce_message();

private:
    long long timestamp;
    mac_addr_t mac_addr;
};


#endif
