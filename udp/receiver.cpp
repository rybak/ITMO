#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include <iostream>
#include <time.h>
#include <vector>
#include <map>

#include "common.h"

#define HEADER_FORMAT "%16s%10s%10s%12"

void print_header()
{
    const char *i = "IP";
    const char *n = "name";
    const char *s = "Student";
    const char *t = "time";
    printf("Current time : %ld\n", time(NULL));
    printf(HEADER_FORMAT"s\n", i, s, n, t);

}
void print_entry(const message_t msg)
{
    struct in_addr tmp;
    tmp.s_addr = msg.ip;
    const char *ip_str = inet_ntoa(tmp);
    int len = strlen(ip_str);
    printf(HEADER_FORMAT"d\n", ip_str,
            msg.student, msg.name, msg.timestamp);
}
int main(int argc, char* argv[])
{
    int sock;
    struct sockaddr_in sock_in;
    make_socket(sock, sock_in, atoi(argv[1]));

    struct timeval tv;
    tv.tv_sec = TIME_INTERVAL;
    tv.tv_usec = 0;
    if (setsockopt(sock,
                SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv)) < 0)
    {
        die("setsockopt : timeval");
    }

    printf("Port %d\n", htons(sock_in.sin_port));

    std::map<int, message_t> clients;
    message_t input;
    socklen_t si_len = sizeof(sock_in);
    while(true)
    {
        memset(&input, 0, sizeof(input));

        int msg_len = recvfrom(sock, &input, sizeof(input), 0,
                (struct sockaddr *) &sock_in, &si_len);
        if (msg_len > 0)
        {
            clients[input.ip] = input;
            clients[input.ip].timestamp = time(NULL);
        } else
        {
            dontdie("recvfrom");
        }
        std::vector<int> to_erase;
        for (auto it = clients.begin();
                it != clients.end(); ++it)
        {
            if (time(NULL) - ((it->second).timestamp) > TIME_GAP)
            {
                to_erase.push_back(it->first);
            }
        }
        for (auto it = to_erase.begin();
                it != to_erase.end(); ++it)
        {
            clients.erase(*it);
        }
        print_header();
        if (clients.empty())
        {
            printf("(NO CLIENTS)\n");
        }
        for (auto it = clients.begin();
                it != clients.end(); ++it)
        {
            print_entry(it->second);
        }
        std::cout << std::endl;
    }
    close(sock);
}


