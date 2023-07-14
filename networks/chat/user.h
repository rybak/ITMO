#ifndef USER_H
#define USER_H

#include <string>

#include "ma.h"
#include "announce_message.h"

struct user
{
    user(const announce_message &);
    void update();

    mac_addr_t mac_addr;
    std::string nickname;
    long long timestamp;
    int ip;
    long long offset;
    bool dead;
    size_t message_sent;
};

#endif

