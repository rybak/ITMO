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
    std::cerr << "sender" << std::endl;
    int sockfd = init_connect_socket(argv[1], argv[2]);
    std::cerr << "sending message" << std::endl;
    write_all(sockfd, SEND_MSG, MSG_SIZE);
    out_ok();
    int token;
    std::cerr << "waiting token" << std::endl;
    read_all(sockfd, (char *) &token, TOKEN_SIZE);
    out_ok();
    std::cerr << "token : " << token << std::endl;
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
        write_all(sockfd, buf, cnt);
        sleep(1);
    }
    std::cerr << "token : " << token << std::endl;
}

