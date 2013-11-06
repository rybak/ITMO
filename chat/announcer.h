#ifndef ANNOUNCER_H
#define ANNOUNCER_H

#include <netinet/in.h>
#include <unistd.h>
#include "message.h"

const uint16_t ANNOUNCE_PORT = 1235;

struct announcer
{
    void announce();
    announcer(const uint16_t port = ANNOUNCE_PORT);
    ~announcer();
private:
    bool good_timing();
    int last_announce_time;
    announce_message msg;

    int announce_sock;
    struct sockaddr_in aa;
};

#endif
