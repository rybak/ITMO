#include <unistd.h>
#include <ctime>
#include <string>

#include <sys/socket.h>
#include <stdlib.h>
#include <cstring>
#include <netdb.h>

#include "chat_message.h"
#include "common.h"
#include "tcp_socket.h"
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
    int conn_sock = accept(sock, NULL, NULL);
    if (conn_sock < 0)
    {
        die("receiver::receive_message : accept");
    }
    int cnt = 0;
    cm_header h;
    cnt = recvfrom(conn_sock, &h, sizeof(h), 0, NULL, NULL);
    if (cnt < 0)
    {
        die("receiver::receive_message : recvfrom header");
    }
    chat_message msg(h);
    msg.to_host();
    if (msg.len > 10000 || msg.len < 0)
    {
        msg.text = "bad len";
        return msg;
    }
    uint32_t len = msg.len;
    char *buffer = new char[len];
    memset(buffer, 0, len);
    cnt = recvfrom(conn_sock, buffer, len, 0, NULL, NULL);
    if (cnt < 0)
    {
        die("receiver::receive_message : recvfrom message");
    }
    buffer[len] = 0;
    msg.text = buffer;
    delete[] buffer;
    return msg;
}

