// sender server.com 1234 < file
// token <- server
// std::cout << token 

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

int main(int argc, char *argv[])
{
    int sockfd = init_connect_socket(argv[1], argv[2]);
    send_all(sockfd, SND_MSG, TYPE_SIZE);
    char buf[BUF_SIZE];
    while (true)
    {
        int cnt = read(0, buf, BUF_SIZE);
        if (cnt == 0)
        {
            break;
        }
        if (cnt < 0)
        {
            perror("read error");
            exit(1);
        }
        send_all(sockfd, buf, cnt);
    }
}

