#ifndef SENDER_H
#define SENDER_H

struct sender
{
    sender();
    ~sender();

    void read_message();
    void cycle();
private:

    bool need_send;
    char *msg;
    void send_message(); // _message
};
#endif
