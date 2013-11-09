#ifndef CHAT_MESSAGE_H
#define CHAT_MESSAGE_H

#include <cstdint>
#include <string>

#include "ma.h"

struct
    __attribute__ ((packed))
cm_header
{
    long long timestamp;
    _mac_addr_t ma;
    int32_t len;
};

struct chat_message
{
    chat_message();
    chat_message(const cm_header &);
    long long timestamp;
    mac_addr_t mac_addr;
    int32_t len;
    std::string text;
    void to_host();
    void to_net();

};

#endif
