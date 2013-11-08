#include <cstdint>
#include <cstring>
#include <cstdio>

#include "unistd.h"
#include <ctime>

#include "chat_message.h"
#include "sender.h"

sender::sender(uint16_t port)
    : port(port)
{
    printf("sender contructor\n");
}

sender::~sender()
{
    close(sock);
}

void sender::send_message(const user &u, const std::string &text)
{
    // connect to all computers
}


