#ifndef ANNOUNCER_H
#define ANNOUNCER_H

#include <netinet/in.h>
#include <unistd.h>
#include "message.h"

#include "ports.h"

struct announcer
{
    void announce();
    announcer(const uint16_t);
    ~announcer();
    int sock;
private:
    bool good_timing();
    int last_announce_time;
    announce_message msg;

    struct sockaddr_in aa;
};

#endif
