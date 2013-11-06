#include <ctime>
#include <cstring>
#include <cstdio>

#include "sender.h"

sender::sender()
{
    printf("sender contructor\n");
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
    printf("Enter message (max length of a message is %ld):\n", MSG_MAX_LEN);
    int cnt = scanf("%s", msg);
    if (cnt < 0)
    {
        perror("scanf gone wrong");
        return;
    }
    // TODO add message to all messages
    need_send = true;
}

