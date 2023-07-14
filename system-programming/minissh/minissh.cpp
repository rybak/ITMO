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
#include <poll.h>
#include <sys/wait.h>

#include <pty.h>

const size_t BUF_SIZE = 4096;

struct buffer_t
{
    char buf[BUF_SIZE];
    bool in_dead;
    bool out_dead;
    size_t pos;
    buffer_t() : in_dead(false), out_dead(false), pos(0)
    {}
};

void propagate_data(pollfd *src, buffer_t *buf, pollfd *dest)
{
    if (buf->pos < BUF_SIZE && (src->revents & POLLIN))
    {
        int cnt = read(src->fd, buf->buf + buf->pos, BUF_SIZE - buf->pos);
        if (cnt == 0)
        {
            std::cerr << "cnt == 0 " << std::endl;
        }
        if (cnt <= 0)
        {
            buf->in_dead = true; // <<<<<
        } else
        {
            buf->pos += cnt;
        }
    }
    if ((buf->pos > 0) && (dest->revents & POLLOUT))
    {
        int cnt = write(dest->fd, buf->buf, buf->pos);
        if (cnt < 0)
        {
            buf->out_dead = true;
        }
        if (cnt > 0)
        {
            memmove(buf->buf, buf->buf + cnt, buf->pos - cnt);
            buf->pos -= cnt;
        }
    }
    if (buf->pos < BUF_SIZE && !buf->in_dead)
    {
        src->events |= POLLIN;
    } else
    {
        src->events &= ~POLLIN;
    }
    //if (buf->out_dead)
    //{
    //    dest->events = 0;
    //}
}
const size_t MAX_PATH = 4096;
void handle_client(int cfd)
{
    int amaster = 0;
    int aslave = 0;
    char name[MAX_PATH];
    if (openpty(&amaster, &aslave, name, NULL, NULL) == -1)
    {
        perror("FAIL : openpty error : ");
        exit(1);
    }
    int shellpid = fork();
    if (shellpid != 0)
    {
        close(aslave);
        pollfd fds[2];
        fds[0].fd = cfd;
        fds[1].fd = amaster;
        fds[0].events = fds[1].events = POLLIN | POLLERR | POLLOUT;
        buffer_t from;
        buffer_t to;
        while (!from.in_dead && !to.in_dead)
        {
            int ready = poll(fds, 2, 0);
            if (!ready)
            {
                continue;
            }
            if (ready < 0)
            {
                if (errno == EINTR)
                {
                    std::cerr << "poll interupted" << std::endl;
                    continue;
                }
                std::cerr << "daemon poll error : " << strerror(errno) << std::endl;
                exit(1);
            }
            propagate_data(&fds[0], &from, &fds[1]);
            propagate_data(&fds[1], &to, &fds[0]);
            std::cerr << from.in_dead << " in  " << to.in_dead << std::endl;
            std::cerr << from.out_dead << " out " << to.out_dead << std::endl;
        }
        exit(0);
    }
    setsid();
    close(amaster);
    close(cfd);
    dup2(aslave, 0);
    dup2(aslave, 1);
    dup2(aslave, 2);
    close(aslave);
    int fd = open(name, O_RDWR);
    close(fd);
    execlp("sh", "sh", NULL);
    exit(0);
}

int main()
{
    int dpid = fork();
    if (dpid != 0)
    {
        waitpid(dpid, NULL, 0);
        exit(0);
    }
    int sid = setsid();
    if (sid < 0)
    {
        perror("FAIL : error creating session ");
        exit(1);
    }
    std::cout << "sid == " << sid << std::endl;
    struct addrinfo hints;
    struct addrinfo *servinfo;
    int status;
    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_UNSPEC; //don’tcareIPv4orIPv6
    hints.ai_socktype = SOCK_STREAM;//TCPstreamsockets
    hints.ai_flags = AI_PASSIVE | AI_ALL; // fillinmyIPforme
    if ((status = getaddrinfo(NULL, "3490", &hints, &servinfo)) != 0){
        fprintf(stderr, "getaddrinfo error:%s\n", gai_strerror(status));
        exit(1);
    }
    std::cerr << reinterpret_cast<long> (servinfo->ai_next) << std::endl;
    struct addrinfo *servinfo_old = servinfo;
    std::vector<pollfd> pollfds;
    
    for (int cnt = 0; servinfo != NULL; servinfo = servinfo -> ai_next)
    {
        std::cerr << cnt << std::endl;
        ++cnt;
        int sockfd = socket(servinfo->ai_family,
                servinfo->ai_socktype, servinfo->ai_protocol);
        if (sockfd < 0)
        {
            std::cerr << "sockfd -1" << std::endl;
            continue;
        }
        int yes = 1;
        if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) < 0)
        {
            std::cerr << "setsockopt -1" << std::endl;
            continue;
        }
        if (bind(sockfd, servinfo->ai_addr, servinfo->ai_addrlen) < 0)
        {
            std::cerr << "bind -1" << std::endl;
            perror(" bind error : ");
            continue;
        }
        if (listen(sockfd, 5) < 0)
        {
            std::cerr << "listen -1" << std::endl;
            continue;
        }
        pollfd tmpfd;
        tmpfd.fd = sockfd;
        tmpfd.events = POLLIN | POLLERR;
        tmpfd.revents = 0;
        pollfds.push_back(tmpfd);
    }
    freeaddrinfo(servinfo_old);
    if (pollfds.empty())
    {
        std::cerr << ("pollfds is empty\n") << std::endl;
        exit(1);
    } else
    {
        std::cout << "pollfds.size == " << pollfds.size() << std::endl;
    }
    while (1)
    {
        int ready = poll(pollfds.data(), pollfds.size(), 0);
        if (!ready)
        {
            continue;
        }
        if (ready < 0)
        {
            if (errno == EINTR)
            {
                std::cerr << "poll interupted" << std::endl;
                continue;
            }
            std::cerr << "poll error : " << strerror(errno) << std::endl;
            exit(1);
        }
        for (int i = 0, n = pollfds.size(); i < n; ++i)
        {
            if (pollfds[i].revents & POLLERR)
            {
                pollfds[i].events = 0;
                close(pollfds[i].fd);
                std::cerr << "FAIL: error while listening socket " << std::endl;
                exit(1);
            }
            if (pollfds[i].revents & POLLIN)
            {
                int sockfd = pollfds[i].fd;
                int cfd = accept(sockfd, NULL, NULL);
                printf("accepted, %d\n", cfd);
                if (cfd < 0)
                {
                    perror("cfd < 0");
                    continue;
                }
                int pid = fork();
                if (pid == 0)
                {
                    for (int i = 0; i < pollfds.size(); ++i)
                    {
                        close(pollfds[i].fd);
                    }
                    handle_client(cfd);
                }
                close(cfd);
            }
        }
    }
}

