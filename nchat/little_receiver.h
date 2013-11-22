#ifndef LITTLE_RECEIVER_H
#define LITTLE_RECEIVER_H

#include "chat_message.h"

struct little_receiver
{
    little_receiver(int);
    ~little_receiver();
    chat_message receive_message();
private:
    int conn_sock;
    char *buffer;
};

#endif

