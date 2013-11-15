#include <unistd.h>
#include <ctime>
#include <string>

#include <stdlib.h>
#include <netdb.h>

#include "chat_message.h"
#include "common.h"
#include "tcp_socket.h"
#include "little_receiver.h"
#include "receiver.h"

receiver::receiver(const uint16_t port)
    : port(port)
{
    struct addrinfo *servinfo;
    sock = tcp_socket(servinfo, port);
    if (sock < 0)
    {
        die("receiver::receiver : socket");
    }
    if (bind(sock, servinfo->ai_addr, servinfo->ai_addrlen) < 0)
    {
        die("receiver::receiver : bind");
    }
    if (listen(sock, 10) < 0)
    {
        die("receiver::receiver : listen");
    }
    freeaddrinfo(servinfo);
}

receiver::~receiver()
{
    close(sock);
}

chat_message receiver::receive_message()
{
    little_receiver lr(sock);
    return lr.receive_message();
}

