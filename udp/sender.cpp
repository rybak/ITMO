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

#define SENDER "sender : "
using namespace std;

void build_message(message_t *msg, in_addr_t ip, 
        const CHAR_TYPE *name, const CHAR_TYPE *student)
{
    msg->ip = ip;
    memset(msg->name, 0, MAX_LEN);
    strncpy(msg->name, name, std::min(MAX_LEN, strlen(name)));
    memset(msg->student, 0, MAX_LEN);
    strncpy(msg->student, student, std::min(MAX_LEN, strlen(student)));
}
int wlan_ip;
void get_wlan0()
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
                int ip = ((in_addr *) tmpAddrPtr) -> s_addr;
                char addressBuffer[INET_ADDRSTRLEN + 1];
                memset(addressBuffer, 0, INET_ADDRSTRLEN + 1);
                inet_ntop(AF_INET, tmpAddrPtr,
                        addressBuffer, INET_ADDRSTRLEN);
                printf("%s IP Address %s = %d\n", ifa->ifa_name, addressBuffer, ip); 
                if (strcmp(ifa->ifa_name, "wlan0") == 0)
                {
                    wlan_ip = ip;
                }

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
    printf("\t%s PORT NAME STUDENT \n", cmd);
}


bool is_new_ip = false;
int new_ip = 0;
void got_new_ip(int n)
{
    is_new_ip = true;
}

int main(int argc, char *argv[])
{
    signal(SIGCHANGEIP, got_new_ip);
    if (argc < 3)
    {
        print_usage(argv[0]);
        die(SENDER"Wrong argv");
    }
    uint16_t port = atoi(argv[1]);
    CHAR_TYPE *name = argv[2];
    CHAR_TYPE *student = argv[3];
    int sock;
    struct sockaddr_in dest;
    make_socket(sock, dest, 0);

    dest.sin_addr.s_addr = htonl(-1);
    dest.sin_port = htons(port);

    get_wlan0();

    int yes = 1;
    if (setsockopt(sock, SOL_SOCKET,
            SO_BROADCAST, &yes, sizeof(int)) < 0)
    {
        die("setsockopt");
    }

    message_t msg;
    int ip = wlan_ip;
    build_message(&msg, ip, name, student);
    size_t msg_size = sizeof(msg);

    for(;;)
    {
        if (is_new_ip)
        {
            FILE *f = fopen("new_ip", "r");
            if (f == NULL)
            {
                die("new_ip file");
            }
            new_ip = 0;
            if (fscanf(f, "%d", &new_ip) != 1)
            {
                die("new_ip scanf");
            }
            ip = new_ip;
            new_ip = 0;
            is_new_ip = false;
            printf(SENDER"NEW IP %d\n", ip);
            build_message(&msg, ip, name, student);
        }
        msg.u.s.a = 0;
        msg.u.s.t = htonl(time(NULL));
        char buf[msg_size];
        memcpy(buf, &msg, msg_size);
        if (sendto(sock, buf, msg_size, 0,
                (struct sockaddr *) &dest, sizeof(dest)) < 0)
        {
            dontdie(SENDER"sendto");
        }
        printf(SENDER"sent\n");
        printf(SENDER"sleep...\n");
        sleep(5);
    } 
    close(sock);
}

