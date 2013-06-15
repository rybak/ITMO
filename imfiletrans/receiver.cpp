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
    std::cout << "receiver" << std::endl;
    int sockfd = init_connect_socket(argv[1], argv[2]);
    std::cout << "sending message" << std::endl;
    send_all(sockfd, RECV_MSG, MSG_SIZE);
    out_ok();
    int token = atoi(argv[3]);
    std::cout << "token : " << token << std::endl;
    std::cout << "sending token" << std::endl;
    send_all(sockfd, (char *) &token, TOKEN_SIZE);
    out_ok();
    char buf[BUF_SIZE];
    while (true)
    {
        int cnt = read(sockfd, buf, BUF_SIZE);
        if (cnt == 0)
        {
            break;
        }
        if (cnt < 0)
        {
            perror("read");
            exit(1);
        }
        int pos = 0;
        while (pos < cnt)
        {
            int wcnt = write(0, buf + pos, cnt - pos);
            pos += wcnt;
        }
    }
}

