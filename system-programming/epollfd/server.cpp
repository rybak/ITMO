#include <stddef.h>
#include <stdlib.h>
#include <errno.h>

#include <string.h>
#include <sys/types.h>
#include <limits.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include <sys/socket.h>
#include <netdb.h>
#include <sys/epoll.h>
#include <sys/wait.h>

#include <iostream>

#include <functional>

#include "http.h"
#include "epollfd.h"

#define ERROR "Server Error : "


#define BUF_SIZE 1024

int main(int argc, char *argv[])
{
    epollfd poll;
    cont_t cont_accept, cont_err, cont_send, cont_recv;
    int listen_sock = init_listen_socket(argv[1], argv[2]);
    cont_err = []()
    {
        std::cerr << "cont_err" << std::endl;
        exit(2);
    };
    int cnt = 0;
    int from = -1, to = -1;
    bool in_pause = false;
    bool out_pause = true;
    cont_accept = [&listen_sock, &cnt, &from, &to, &poll,
                &in_pause, &out_pause,
                &cont_send, &cont_recv, &cont_err]()
    {
        int conn_socket = accept(listen_sock, NULL, NULL);
        if (conn_socket < 0)
        {
            perror("accept");
            exit(1);
        }
        if (cnt == 0)
        {
            from = conn_socket;
            std::cerr << "from == " << from << std::endl;
            poll.subscribe(from, EPOLLIN, cont_recv, cont_err);
            cnt = 1;
        } else if (cnt == 1)
        {
            to = conn_socket;
            std::cerr << "to == " << to << std::endl;
            poll.subscribe(to, EPOLLOUT, cont_send, cont_err);
            out_pause = false;
            cnt = 2;
        } else {
            std::cerr << "ololo error" << std::endl;
            exit(2);
        }
    };
    bool in_done = false;
    bool out_done = false;
    int in_pos = 0;
    char buf[BUF_SIZE];
    cont_recv = [&from, &to, &buf, &in_pos, &poll,
                &in_done, &out_done,
                &in_pause, &out_pause,
                &cont_send, &cont_recv, &cont_err]()
    {
        int cnt = read(from, buf + in_pos, BUF_SIZE - in_pos);
        if (cnt < 0)
        {
            perror("read");
            exit(1);
        }
        if (cnt == 0)
        {
            poll.unsubscribe(from, EPOLLIN);
            in_done = true;
            return;
        }
        in_pos += cnt;
        if (out_pause)
        {
            if (to > 0)
            {
                poll.subscribe(to, EPOLLOUT, cont_send, cont_err);
                out_pause = false;
            }
        }
        if (in_pos == BUF_SIZE)
        {
            poll.unsubscribe(from, EPOLLIN);
            in_pause = true;
        }
    };
    cont_send = [&from, &to, &buf, &in_pos, &poll,
                &in_done, &out_done,
                &in_pause, &out_pause,
                &cont_send, &cont_recv, &cont_err]()
    {
        int cnt = write(to, buf, in_pos);
        if (cnt < 0)
        {
            perror("write");
            exit(1);
        }
        if (cnt > 0)
        {
            memmove(buf, buf + cnt, BUF_SIZE - in_pos - cnt);
            in_pos -= cnt;
        }
        if (in_pos == 0 && in_done)
        {
            poll.unsubscribe(to, EPOLLOUT);
            out_done = true;
        }
        if (in_pos < BUF_SIZE && in_pause)
        {
            poll.subscribe(from, EPOLLIN, cont_recv, cont_err);
            in_pause = false;
        }
        if (cnt == 0)
        {
            out_pause = true;
            poll.unsubscribe(to, EPOLLOUT);
        }
    };
    int dpid = fork();
    if (dpid != 0)
    {
        waitpid(dpid, NULL, 0);
        exit(0);
    }
    int sid = setsid();
    if (sid < 0)
    {
        perror(ERROR"creating session ");
        exit(1);
    }
    std::cerr << "server sid == " << sid << std::endl;
    poll.subscribe(listen_sock, EPOLLIN, cont_accept, cont_err);
    while (!out_done)
    {
        poll.cycle();
        if (from > 0)
        {
        }

    }
    exit(EXIT_SUCCESS);
}
