#ifndef ANNOUNCER_H
#define ANNOUNCER_H

#include <netinet/in.h>
#include <unistd.h>
#include "message.h"

#include "ports.h"

struct announcer
{
    void announce();
    announcer(const uint16_t port = UDP_PORT);
    ~announcer();
private:
    bool good_timing();
    int last_announce_time;
    announce_message msg;

    int announce_sock;
    struct sockaddr_in aa;
};

#endif
