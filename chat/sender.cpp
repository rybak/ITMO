#include <ctime>
#include <cstring>

// future includes : 
#include <cstdio>
// /future

#include "sender.h"
sender::sender()
{
}

sender::~sender()
{
}

void sender::cycle()
{
    if (need_send)
    {
        send_message();
    }
}

void sender::send_message()
{
    // connect to all computers
    need_send = false;
}

void sender::read_message()
{
    msg = new char[MSG_MAX_LEN];
    printf("Enter message (max length of a message is %d):\n", MSG_MAX_LEN);
    scanf("%s", msg);
    // TODO add message to all messages
    need_send = true;
}

