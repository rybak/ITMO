#ifndef RECEIVER_H
#define RECEIVER_H

#include <cstdint>
#include <netinet/in.h>

struct receiver
{
    receiver(const uint16_t);
    ~receiver();
    void cycle();
    int sock = -1;
private:
    sockaddr_in lsa;
    uint16_t port;

    void receive_messages();
    void print_messages();
    void add_message(char *, size_t);
};

#endif
