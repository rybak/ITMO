#ifndef CHATTER_H
#define CHATTER_H

#include <netinet/in.h>
#include <unistd.h>
#include "message.h"

#include "sender.h"
#include "receiver.h"
#include "ports.h"

struct chatter
{
    chatter(const uint16_t port = TCP_PORT);
    void cycle();
    ~chatter();
    void read_message();
private:
    sender s;
    receiver r;
};

#endif
