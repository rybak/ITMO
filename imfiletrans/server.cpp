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
#include <map>

#include <pty.h>

#include "http.h"
#include "client.h"
std::map<int, client> clients;

int process_event(int epollfd, int conn_sock, int events)
{
    struct epoll_event ev;
    setnonblocking(conn_sock);
    ev.events = (EPOLLIN | EPOLLOUT) & events;
    ev.data.fd = conn_sock;
    if (epoll_ctl(epollfd, EPOLL_CTL_ADD, conn_sock, &ev) == -1)
    {
        perror("epoll_ctl: conn_sock");
        exit(EXIT_FAILURE);
    }
    clients[conn_sock] = client(events);
}

int main(int argc, char *argv[])
{
    std::cout << "Server " << std::endl;
    int sockfd = init_listen_socket(argv[1], argv[2]);
    struct epoll_event ev, events[MAX_EVENTS];
    int listen_sock = sockfd, conn_sock, nfds, epollfd;
    epollfd = epoll_create(42); // ... and such things
    if (epollfd == -1)
    {
        perror("epoll_create");
        exit(1);
    }
    ev.events = EPOLLIN | EPOLLOUT | EPOLLERR;
    ev.data.fd = listen_sock;
    if (epoll_ctl(epollfd, EPOLL_CTL_ADD, listen_sock, &ev) == -1)
    {
        perror("epoll_ctl: listen_sock");
        exit(1);
    }
    while (true)
    {
        nfds = epoll_wait(epollfd, events, MAX_EVENTS, -1);
        if (nfds == -1)
        {
            perror("epoll_pwait");
            exit(EXIT_FAILURE);
        }
        for (i = 0; i < nfds; ++i)
        {
            if (events[i].data.fd == listen_sock)
            {
                conn_sock = accept(listen_sock, NULL, NULL);
                if (conn_sock == -1)
                {
                    perror("accept");
                    exit(EXIT_FAILURE);
                }
                process_event(epolldf, conn_sock, events[i].events);
            } else
            {
                clients[events[i].data.fd].process_client();
            }
        }
    }
    return 0;
}
