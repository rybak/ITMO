#ifndef SENDER_H
#define SENDER_H

struct sender
{
    sender(const uint16_t);
    ~sender();

    void read_message();
    void cycle();
    int sock = -1;
private:

    bool need_send;
    char *msg;
    void send_message(); // _message

    uint16_t port;
};
#endif
