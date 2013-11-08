#ifndef SENDER_H
#define SENDER_H

#include <string>

#include "user.h"

struct sender
{
    sender(const uint16_t);
    ~sender();

    int sock = -1;
    void send_message(const user &, const std::string &); // _message

private:

    uint16_t port;
};
#endif
