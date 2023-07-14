#ifndef LISTENER_H
#define LISTENER_H
#include <netinet/in.h>

#include "ma.h"
#include "announce_message.h"

struct listener
{
    listener(const uint16_t);
    ~listener();
    announce_message receive_message();
    int sock;
private:
    uint16_t port;
    struct sockaddr_in sock_in;
    socklen_t si_len;
};

#endif

