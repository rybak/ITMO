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
#include <unordered_map>

#include "client.h"

#define ERROR "Error : "

std::unordered_map<int, client>& clients()
{
    static auto res = std::unordered_map<int, client>();
    return res;
}

int epollfd;
int listen_sock;

void process_event(int events)
{
    int conn_sock = accept(listen_sock, NULL, NULL);
    if (conn_sock < 0)
    {
        perror(ERROR"accept");
        exit(EXIT_FAILURE);
    }
    std::cerr << "\tprocess_event fd == " << conn_sock << std::endl;
    epoll_event ev;
    // setNonblocking(conn_sock);
    ev.events = (EPOLLIN | EPOLLOUT) & events;
    ev.data.fd = conn_sock;
    if (epoll_ctl(epollfd, EPOLL_CTL_ADD, conn_sock, &ev) == -1)
    {
        perror("epoll_ctl: conn_sock");
        exit(EXIT_FAILURE);
    }
    clients().emplace(conn_sock, client(events, conn_sock));
}

void mod(int fd, int mode)
{
    epoll_event ev;
    ev.data.fd = fd;
    ev.events = mode;
    if (epoll_ctl(epollfd, EPOLL_CTL_MOD, fd, &ev) < 0)
    {
        perror("epoll_ctl");
        exit(EXIT_FAILURE);
    }
}

const size_t MAX_EVENTS = 10;

void init(char *argv[])
{
    listen_sock = init_listen_socket(argv[1], argv[2]);
    epollfd = epoll_create1(0);
    if (epollfd == -1)
    {
        perror("epoll_create");
        exit(EXIT_FAILURE);
    }
    epoll_event ev;
    ev.events = EPOLLIN | EPOLLOUT | EPOLLERR;
    ev.data.fd = listen_sock;
    if (epoll_ctl(epollfd, EPOLL_CTL_ADD, listen_sock, &ev) == -1)
    {
        perror("epoll_ctl: add listen_sock");
        exit(EXIT_FAILURE);
    }
}

void process_client(int id)
{
    clients().at(id).process_client();
    token_t t = clients().at(id).token;
    if (clients().at(id).wake_up != WRONG_FD)
    {
        int fd = clients().at(id).wake_up;
        std::cerr << "\twake up " << fd << std::endl;
        mod(fd, EPOLLIN | EPOLLOUT | EPOLLERR);
    }
    switch(clients().at(id).state)
    {
    case client::DEAD:
        std::cerr << "\tdead " << id << std::endl;
        epoll_ctl(epollfd, EPOLL_CTL_DEL, id, NULL);
        close(id);
        if (buffers().count(t))
        {
            switch(clients().at(id).type)
            {
            case client::SENDER:
                buffers().at(t).sender_dead = true;
                buffers().at(t).sender_sock = WRONG_FD;
                break;
            case client::RECEIVER:
                buffers().at(t).receiver_dead = true;
                buffers().at(t).receiver_sock = WRONG_FD;
                break;
            }
            if (buffers().at(t).sender_dead
                && buffers().at(t).receiver_dead)
            {
                buffers().erase(t);
            }
        }
        clients().erase(id);
        break;
    case client::SENDING_TOKEN:
    case client::SENDING_FILE:
        mod(id, EPOLLOUT | EPOLLERR);
        break;
    case client::RECEIVING_MSG:
    case client::RECEIVING_TOKEN:
    case client::RECEIVING_FILE:
        mod(id, EPOLLIN | EPOLLERR);
        break;
    }
    if (clients().count(id))
    {
        if (clients().at(id).pause)
        {
            std::cerr << "\tpause " << id << std::endl;
            mod(id, EPOLLERR);
        }
    }
}

int main(int argc, char *argv[])
{
    std::cerr << "server" << std::endl;
    init(argv);
    while (true)
    {
        std::cerr << "epoll_wait" << std::endl;
        epoll_event events[MAX_EVENTS];
        int nfds = epoll_wait(epollfd, events, MAX_EVENTS, -1);
        if (nfds == -1)
        {
            perror(ERROR"epoll_pwait");
            exit(EXIT_FAILURE);
        }
        for (int i = 0; i < nfds; ++i)
        {
            if (events[i].data.fd == listen_sock)
            {
                process_event(events[i].events);
            } else
            {
                process_client(events[i].data.fd);
            }
        }
    }
    return 0;
}
