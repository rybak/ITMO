#include <cstdint>
#include <cstring>

#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <netinet/in.h>
#include <ctime>
#include <iostream>

#include "chat_message.h"
#include "common.h"
#include "tcp_socket.h"
#include "sender.h"

sender::sender(uint16_t port)
    : port(port)
{
}

sender::~sender()
{
    close(sock);
}

void sender::send_message(const user &u, const std::string &text)
{
    struct addrinfo *servinfo;
    int sock = tcp_socket(servinfo, port);
    freeaddrinfo(servinfo);
    if (sock < 0)
    {
        die("sender::send_message : socket");
    }
    sockaddr_in addr = {AF_INET, htons(port), 0, 0};
    addr.sin_addr.s_addr = u.ip;
    if (connect(sock, (sockaddr *) &addr, sizeof(addr)) < 0)
    {
        dontdie("sender::send_message : connect");
        close(sock);
        return;
    }
    chat_message msg(text);
    msg.to_user_time(u.offset);
    msg.to_net();
    cm_header h;
    make_header(h, msg);
    char *buffer = new char[text.length() + sizeof(h) ];
    memcpy(buffer, &h, sizeof(h));
    memcpy(buffer + sizeof(h), text.data(), text.length());
    socklen_t si_len = sizeof(addr);
    int cnt = sendto(sock, buffer, msg.len + sizeof(h), 0, (struct sockaddr *) &addr, si_len);
    if (cnt < 0)
    {
        std::cerr << "send to error" << std::endl;
    }
    close(sock);
}

