#ifndef SENDER_H
#define SENDER_H

struct sender
{
    sender(const uint16_t);
    ~sender();

    int sock = -1;
    void send_message(int); // _message

private:

    uint16_t port;
};
#endif
