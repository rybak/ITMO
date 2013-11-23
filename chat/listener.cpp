#include <netinet/in.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <ctime>

#include <iostream>

#include "ports.h"
#include "listener.h"
#include "common.h"

listener::listener(const uint16_t port = UDP_PORT)
    : port(port)
{
    make_udp_socket(sock, sock_in, port);
    struct timeval tv;
    tv.tv_sec = 0;
    tv.tv_usec = 10000;
    if (setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv)) < 0)
    {
        die("listener : setsockopt : timeval");
    }
    si_len = sizeof(sock_in);
}

listener::~listener()
{
    close(sock);
}

announce_message listener::receive_message()
{
    packed_message pmsg;
    int msg_len = recvfrom(sock, &pmsg, sizeof(pmsg), 0,
            (struct sockaddr *) &sock_in, &si_len);
    if (msg_len < 0)
    {
        die("listener::receive_message");
    }
    int new_ip = sock_in.sin_addr.s_addr;
    announce_message amsg(pmsg);
    amsg.ip = new_ip;
    amsg.to_host();
    return amsg;
}

