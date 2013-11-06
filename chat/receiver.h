#ifndef RECEIVER_H
#define RECEIVER_H

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
