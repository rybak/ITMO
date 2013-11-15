#ifndef SENDER_H
#define SENDER_H

#include <string>

#include "user.h"

struct sender
{
    sender(const uint16_t);
    ~sender();

    void send_message(const user &, const std::string &, long long);

private:

    int sock;
    uint16_t port;
};
#endif
