#include <cstdint>
#include <cstring>
#include <cstdio>

#include <unistd.h>
#include <netinet/in.h>
#include <ctime>

// #include "chat_message.h"
#include "common.h"
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
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0)
    {
        die("sender::send_message : socket");
    }
    sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = port;
    addr.sin_addr.s_addr = u.ip;
    if (connect(sock, (sockaddr *) &addr, sizeof(addr)) < 0)
    {
        die("sender::send_message : connect");
    }
}


