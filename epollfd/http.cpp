#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <fcntl.h>

#include <iostream>
#include <sys/socket.h>
#include <netdb.h>
#include <sys/epoll.h>
#include <sys/wait.h>

void out_ok(int level)
{
    for (int i = 0; i < level; ++i)
    {
        std::cerr << '\t';
    }
    std::cerr << "...ok" << std::endl;
}

int write_all(int fd, const char *buf, int n)
{
    int pos = 0;
    while (pos < n)
    {
        int cnt = write(fd, buf + pos, n - pos);
        if (cnt < 0)
        {
            perror("write_all");
            exit(1);
        }
        if (cnt > 0)
        {
            pos += cnt;
        }
    }
    return 0;
}

int read_all(int fd, char *buf, int n)
{
    int pos = 0;
    while (pos < n)
    {
        int cnt = read(fd, buf + pos, n - pos);
        if (cnt < 0)
        {
            perror("recv_all");
            exit(1);
        }
        if (cnt > 0)
        {
            pos += cnt;
        }
    }
    return 0;
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
        perror("socket");
        exit(1);
    }
    return sockfd;
}

int init_connect_socket(char *node, char *service)
{
    struct addrinfo *servinfo;
    int sockfd = init_socket(node, service, &servinfo);
    int yes = 1;
    if (setsockopt(sockfd, SOL_SOCKET,
                SO_REUSEADDR, &yes, sizeof(int)) < 0)
    {
        perror("setsockopt");
        exit(1);
    }
    if (connect(sockfd, servinfo->ai_addr, servinfo->ai_addrlen) < 0)
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
        perror("setsockopt");
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


