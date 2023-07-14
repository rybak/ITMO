#include <cstring>

#include <arpa/inet.h>

#include "common.h"

#include "udp.h"

void make_udp_socket(int &fd, struct sockaddr_in &sock, uint16_t port)
{
    fd = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (fd < 0)
    {
        die("socket");
    }
    socklen_t si_len = sizeof(sock);
    memset(&sock, 0, si_len);
    sock.sin_family = AF_INET;
    sock.sin_port = htons(port);
    sock.sin_addr.s_addr = htonl(INADDR_ANY);

    int status = bind(fd, (struct sockaddr *) &sock, si_len);
    if (status < 0)
    {
        die("bind");
    }
}


