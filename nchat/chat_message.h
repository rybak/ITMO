#ifndef CHAT_MESSAGE_H
#define CHAT_MESSAGE_H

#include <string>

#include "chat.h"
#include "ma.h"

struct
    __attribute__ ((packed))
cm_header
{
    long long timestamp;
    _mac_addr_t ma;
    uint32_t len;
};

struct chat_message
{
    chat_message(const std::string &, chat_time_t);
    chat_message(const cm_header &);
    void to_user_time(chat_time_t);
    long long timestamp;
    mac_addr_t mac_addr;
    std::string text;
    uint32_t len;
    void to_host();
    void to_net();

};

void make_header(cm_header &, const chat_message &);

#endif
