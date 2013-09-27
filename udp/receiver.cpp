#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <poll.h>

#include <iostream>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include "common.h"

int main(int argc, char* argv[])
{
    int sock;

    sock = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (sock < 0)
    {
        die("socket");
    }
      
    struct sockaddr_in sock_in;
    socklen_t si_len = sizeof(struct sockaddr_in);
    memset(&sock_in, 0, si_len);
    sock_in.sin_addr.s_addr = htonl(INADDR_ANY);
    sock_in.sin_port = htons(atoi(argv[1]));
    sock_in.sin_family = AF_INET;

    int status = bind(sock, (struct sockaddr *) &sock_in,
            si_len);
    if (status < 0)
    {
        die("bind");
    }

    printf("Port %d\n", htons(sock_in.sin_port));

    size_t buflen = MAXBUF;
    char buffer[MAXBUF];
    while(true)
    {
        memset(buffer, 0, buflen);
        int msg_len = recvfrom(sock, buffer, buflen, 0,
                (struct sockaddr *) &sock_in, &si_len);
        printf("message length = %d\n", msg_len);
        if (msg_len > 0)
        {
            printf("message: %s\n", buffer);
            if(strncmp(buffer, "end", 3) == 0)
            {
                break;
            }
        } else
        {
            dontdie("recvfrom");
        }
    }
    close(sock);
}


