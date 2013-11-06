#ifndef CHATTER_H
#define CHATTER_H

#include <netinet/in.h>
#include <unistd.h>
#include "message.h"

const uint16_t ANNOUNCE_PORT = 1234;

struct chatter
{
    chatter(const uint16_t port = TCP_LISTEN_PORT);
    ~chatter();
private:
    bool good_timing();
    int last_announce_time;
    announce_message msg;

    int listen_sock;
    struct sockaddr_in aa;
};

#endif
