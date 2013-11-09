#ifndef RECEIVER_H
#define RECEIVER_H

#include <cstdint>
#include <netinet/in.h>

#include "chat_message.h"

struct receiver
{
    receiver(const uint16_t);
    ~receiver();
    int sock = -1;
    chat_message receive_message();
private:
    uint16_t port;
    struct sockaddr_in sock_in;
    socklen_t si_len;
};

#endif
