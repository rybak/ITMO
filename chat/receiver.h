#ifndef RECEIVER_H
#define RECEIVER_H

#include <cstdint>
#include <netinet/in.h>

struct receiver
{
    receiver(const uint16_t);
    ~receiver();
    int sock = -1;
    void receive_message();
private:
    uint16_t port;

};

#endif
