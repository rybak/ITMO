#ifndef RECEIVER_H
#define RECEIVER_H

#include <cstdint>
#include <netinet/in.h>

const uint16_t TCP_LISTEN_PORT = 1236;

struct receiver
{
    receiver();
    ~receiver();
    void cycle();
private:
    int listen_sock;
    sockaddr_in lsa;
    void receive_messages();
    void print_messages();
    void add_message(char *, size_t);
};

#endif
