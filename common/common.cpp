#include <sys/socket.h>
#include <netdb.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <iostream>
#include <algorithm>

#include <ifaddrs.h>
#include <time.h>

#include "common.h"

void die(const char *s)
{
    perror(s);
    exit(1);
}

void dontdie(const char *s)
{
    perror(s);
    printf("\n");
}

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

long long host_time()
{
    long long res = time(NULL) * 1000ll;
    return res;
}

std::string ip_string(const int ip)
{
    char ip_str[1024];
    inet_ntop(AF_INET, &ip, ip_str, sizeof(ip_str));
    return std::string(ip_str);
}

std::string time_string(const long long t)
{
    static const size_t TIME_BUF_LEN = 100;
    static const char *TIME_FORMAT = "%Y:%m:%d %H:%M:%S";
    char time_buf[TIME_BUF_LEN];
    time_t timestamp = t / 1000l;
    tm *tm_info = localtime(&timestamp);
    strftime(time_buf, TIME_BUF_LEN, TIME_FORMAT, tm_info);
    return std::string(time_buf);
}
