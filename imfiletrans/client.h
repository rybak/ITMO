#ifndef CLIENT_H
#define CLIENT_H

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


#include "http.h"

typedef int token_t;
const int WRONG_FD = -1;
struct async_buf
{
    async_buf() : fd(WRONG_FD), buf(NULL), n(0)
    {}

    async_buf(int fd, char *buf, int cnt)
        : fd(fd), buf(buf), n(cnt)
    {}

    int read()
    {
        if (n == 0)
        {
            return 0;
        }
        int cnt = ::read(fd, (buf + pos), n);
        shift(cnt, "async_buf::read");
        return cnt;
    }

    int write()
    {
        if (n == 0)
        {
            return 0;
        }
        int cnt = ::write(fd, buf + pos, n);
        shift(cnt, "async_buf::write");
        return cnt;
    }

private:
    int fd;
    size_t pos, n;
    char *buf;

    void shift(int cnt, const char *msg)
    {
        if (cnt > 0)
        {
            n -= cnt;
            pos += cnt;
        }
        if (cnt < 0)
        {
            perror(msg);
            exit(1);
        }
    }
};

struct client
{
    client(int, int);
    client() : state(UNDEFINED), fd(WRONG_FD)
    {}
    void process_client();

private:
    enum state_t
    {
        RECEIVING_MSG, RECEIVING_TOKEN, SENDING_TOKEN,
        RECEIVING_FILE, SENDING_FILE, UNDEFINED
    } state;
    enum client_type_t { SENDER, RECEIVER } type;
    token_t token;
    int fd;
    async_buf buf;
    char msg[MSG_SIZE];

    void check_msg();
    int create_token();
    void process_token();
};


#endif
