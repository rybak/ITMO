#ifndef CHATTER_H
#define CHATTER_H

#include <netinet/in.h>
#include <unistd.h>
#include "message.h"

#include "sender.h"
#include "receiver.h"

const size_t MSG_MAX_LEN = 1024;

struct chatter
{
    chatter(const uint16_t port = TCP_LISTEN_PORT);
    void cycle();
    ~chatter();
private:
    sender s;
    receiver r;
};

#endif
