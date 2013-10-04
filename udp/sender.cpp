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

#include <iostream>
#include <algorithm>

#include <ifaddrs.h>
#include <time.h>


#include "common.h"

using namespace std;

void build_message(message_t *msg, const char *ip_str, 
        const char *name, const char *student)
{
    msg->ip = inet_addr(ip_str);
    if (msg->ip == -1)
    {
        std::cerr << ip_str << std::endl;
        die("inet_addr");
    }
    memset(msg->name, 0, MAX_LEN);
    strncpy(msg->name, name, std::min(MAX_LEN, strlen(name)));
    memset(msg->student, 0, MAX_LEN);
    strncpy(msg->student, student, std::min(MAX_LEN, strlen(student)));
}

void print_ip()
{
    struct ifaddrs *ifAddrStruct = NULL;
    struct ifaddrs *ifa = NULL;
    void *tmpAddrPtr = NULL;
    if (getifaddrs(&ifAddrStruct) < 0)
    {
        die("getifaddrs");
    }
    for (ifa = ifAddrStruct; ifa != NULL; ifa = ifa->ifa_next)
    {
        if (ifa->ifa_addr != NULL)
        {
            if (ifa->ifa_addr->sa_family == AF_INET)
            {
                tmpAddrPtr = &(((struct sockaddr_in *) ifa->ifa_addr)
                        ->sin_addr);
                char addressBuffer[INET_ADDRSTRLEN + 1];
                memset(addressBuffer, 0, INET_ADDRSTRLEN + 1);
                inet_ntop(AF_INET, tmpAddrPtr,
                        addressBuffer, INET_ADDRSTRLEN);
                printf("%s IP Address %s\n", ifa->ifa_name, addressBuffer); 
            } else if (ifa->ifa_addr->sa_family == AF_INET6) {
                tmpAddrPtr = &(((struct sockaddr_in6 *) ifa->ifa_addr)
                        ->sin6_addr);
                char addressBuffer[INET6_ADDRSTRLEN + 1];
                memset(addressBuffer, 0, INET6_ADDRSTRLEN + 1);
                inet_ntop(AF_INET6, tmpAddrPtr,
                        addressBuffer, INET6_ADDRSTRLEN);
                printf("%s IP Address %s\n",
                        ifa->ifa_name, addressBuffer); 
            } 
        }
    }
    if (ifAddrStruct!= NULL)
    {
    struct sockaddr_in dest;
        freeifaddrs(ifAddrStruct);
    }
}

void print_usage(char *cmd)
{
    printf("Usage:\n");
    printf("\t%s PORT IP NAME STUDENT \n", cmd);
}

int main(int argc, char *argv[])
{
    if (argc < 4)
    {
        print_usage(argv[0]);
        die("Wrong arguments");
    }
    int sock;
    struct sockaddr_in dest;
    make_socket(sock, dest, 0);

    dest.sin_addr.s_addr = htonl(-1);
    dest.sin_port = htons(atoi(argv[1]));

    print_ip();

    int yes = 1;
    if (setsockopt(sock, SOL_SOCKET,
            SO_BROADCAST, &yes, sizeof(int)) < 0)
    {
        die("setsockopt");
    }

    message_t msg;
    build_message(&msg, argv[2], argv[3], argv[4]);
    size_t msg_size = sizeof(msg);

    for(;;)
    {
        msg.timestamp=time(NULL);
        char buf[msg_size];
        memcpy(buf, &msg, msg_size);
        if (sendto(sock, buf, msg_size, 0,
                (struct sockaddr *) &dest, sizeof(dest)) < 0)
        {
            dontdie("sendto");
        }
        sleep(5);
    } 
    close(sock);
}

