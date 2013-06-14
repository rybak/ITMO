#ifndef HTTP11
#define HTTP11

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <fcntl.h>

#include <iostream>
#include <vector>

#include <sys/socket.h>
#include <netdb.h>
#include <sys/epoll.h>
#include <sys/wait.h>

const size_t BUF_SIZE = 4096;
const size_t MSG_SIZE = 5;
const size_t TOKEN_SIZE = sizeof(int);
const char *SEND_MSG = "send";
const char *RECV_MSG = "recv";

int do_all(int fd, const char *buf, int n, int (*f)(int, char*, int))
{
    while (n > 0)
    {
        int cnt = f(fd, buf, n);
        if (cnt < 0)
        {
            perror("do_all");
            exit(1);
        }
        if (cnt > 0)
        {
            buf += cnt;
            n -= cnt;
        }
    }
    return 0;
}

int send_all(int fd, const char *buf, int n)
{
    do_all(fd, buf, n, write);
}

int recv_all(int fd, const char *buf, int n)
{
    do_all(fd, buf, n, read);
}

int init_socket(char *node, char *service, struct addrinfo **servinfo)
{
    struct addrinfo hints;
    int status;
    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_UNSPEC; // don’t care IPv4 or IPv6
    hints.ai_socktype = SOCK_STREAM; // TCP stream sockets
    hints.ai_flags = AI_PASSIVE | AI_ALL; // fill in my IP for me
    if ((status =
        getaddrinfo(node, service, &hints, servinfo)) != 0)
    {
        std::cerr << "getaddrinfo error:\n" <<
            gai_strerror(status) << std::endl;
        exit(1);
    }
    int sockfd = socket((*servinfo)->ai_family,
            (*servinfo)->ai_socktype, (*servinfo)->ai_protocol);
    if (sockfd < 0)
    {
        perror("sockfd -1\n");
        exit(1);
    }
    return sockfd;
}

int init_connect_socket(char *node, char *service)
{
    struct addrinfo *servinfo;
    int sockfd = init_socket(node, service, &servinfo);
    if (connect(sockfd, servinfo->ai_addr, servinfo->ai_addrlen) == -1)
    {
        perror("connect");
        exit(1);
    }
    freeaddrinfo(servinfo);
    return sockfd;
}

int init_listen_socket(char *node, char *service)
{
    struct addrinfo *servinfo;
    int sockfd = init_socket(node, service, &servinfo);
    int yes = 1;
    if (setsockopt(sockfd, SOL_SOCKET,
        SO_REUSEADDR, &yes, sizeof(int)) < 0)
    {
        
        std::cerr << "setsockopt" << std::endl;
        exit(1);
    }
    if (bind(sockfd, servinfo->ai_addr, servinfo->ai_addrlen) < 0)
    {
        perror("bind");
        exit(1);
    }
    if (listen(sockfd, 5) < 0)
    {
        perror("listen");
        exit(1);
    }
    freeaddrinfo(servinfo);
    return sockfd;
}

#endif

