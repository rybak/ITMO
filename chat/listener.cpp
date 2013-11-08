#include <netinet/in.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <ctime>

#include <cstring>
#include <cstdio>

#include "ports.h"
#include "listener.h"
#include "common.h"

listener::listener(const uint16_t port = UDP_PORT)
    : port(port)
{
    printf("listener contructor\n");
    make_udp_socket(sock, sock_in, port);
    struct timeval tv;
    tv.tv_sec = 0;
    tv.tv_usec = 10000;
    if (setsockopt(sock,
                SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv)) < 0)
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
    packed_message msg;
    int msg_len = recvfrom(sock, &msg, sizeof(msg), 0,
            (struct sockaddr *) &sock_in, &si_len);
    if (msg_len == -1)
    {
        // dontdie("listener :: recvfrom");
        return;
    }
    int new_ip = sock_in.sin_addr.s_addr;
    char ip_str[1024];
    inet_ntop(AF_INET, &(sock_in.sin_addr.s_addr), ip_str, sizeof(ip_str));
    printf("ip = '%s' == %d\n", ip_str, new_ip);
    announce_message amsg(msg);
    amsg.to_host();
    return amsg;
}


