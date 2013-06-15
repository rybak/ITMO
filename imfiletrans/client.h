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
struct client
{
    void process_client();
    client(int, int);
    client();
    ~client();
    client(const client&);
    bool pause;
    int wake_up;
    enum state_t
    {
        RECEIVING_MSG, RECEIVING_TOKEN, SENDING_TOKEN,
        RECEIVING_FILE, SENDING_FILE, UNDEFINED, DEAD
    } state;
    void test();

private:
    enum client_type_t { SENDER, RECEIVER } type;
    token_t token;
    int fd;

    char *msg;
    size_t msg_pos, token_pos;
    void default_values();
    void read_msg();
    void read_token();
    void write_token();
    
    void check_msg();
    int create_token();
    void process_token();
};


#endif
