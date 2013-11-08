#ifndef USER_H
#define USER_H

#include <string>

#include "ma.h"

struct user
{
    user(const announce_message &);

    void update();
    mac_addr_t mac_addr;
    std::string nickname;
    int ip;
    long long timestamp;
    long long offset;
};

#endif
