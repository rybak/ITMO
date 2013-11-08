#include <cstdint>
#include <ctime>
#include <cstring>
#include <cstdio>

#include "sender.h"
#include "message.h"

sender::sender(uint16_t port)
    : port(port)
{
    printf("sender contructor\n");
}

sender::~sender()
{
}

void sender::send_message(int ip)
{
    // connect to all computers
}


