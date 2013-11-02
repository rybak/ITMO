#ifndef ANNOUNCER_H
#define ANNOUNCER_H

#include "message.h"

const uint16_t ANNOUNCE_PORT = 1234;

struct announcer
{
    void announce();
    announcer();
    ~announcer();
private:
    bool good_timing();
    int last_announce_time;
    announce_message msg;

    int announce_sock;
    struct sockaddr_in announce_addr;
    
};

#endif
