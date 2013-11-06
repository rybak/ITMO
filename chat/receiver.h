#ifndef RECEIVER_H
#define RECEIVER_H

const uint16_t TCP_LISTEN_PORT = 1236;

struct receiver
{
    receiver();
    ~receiver();

private:
    int listen_sock;
    struct sockaddr_in lsa;
    void receive_messages();
    void print_messages();

};

#endif
