#ifndef ANNOUNCER_H
#define ANNOUNCER_H

const uint16_t ANNOUNCE_PORT = 1234;

struct announcer
{
    void announce();
    announcer();
private:
    int last_announce_time;

    int announce_sock;
    struct sockaddr_in announce_addr;

};

#endif
