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
#include <ctime>
#include <endian.h>

#include "common.h"
#include "chat.h"

using namespace std;

const uint16_t PORT = 1235;

void build_message(message_t *msg, in_addr_t ip, 
        const CHAR_TYPE *name, const CHAR_TYPE *student)
{
    msg->ip = ip;
    memset(msg->name, 0, MAX_LEN);
    strncpy(msg->name, name, std::min(MAX_LEN, strlen(name)));
    memset(msg->student, 0, MAX_LEN);
    strncpy(msg->student, student, std::min(MAX_LEN, strlen(student)));
}

void print_usage(char *cmd)
{
    printf("Usage:\n");
    printf("\t%s \n", cmd);
}

long long net_time()
{
    long long res = time(NULL) * 1000 + (rand() % 1000);
    res = htobe64(res);
    return res;
}

int main(int argc, char *argv[])
{
    // if (argc < 3)
    // {
    //     print_usage(argv[0]);
    //     die(SENDER"Wrong argv");
    // }

    make_udp_socket(announce_sock, announce_addr, 0);

    announce_addr.sin_addr.s_addr = htonl(-1);
    announce_addr.sin_port = htons(port);

    int yes = 1;
    if (setsockopt(announce_sock, SOL_SOCKET,
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
        msg.timestamp = time_to_net();
        char buf[msg_size];
        memcpy(buf, &msg, msg_size);
        if (sendto(announce_sock, buf, msg_size, 0,
                (struct sockaddr *) &announce_addr, sizeof(announce_addr)) < 0)
        {
            dontdie(SENDER"sendto");
        }
        printf(SENDER"sent\n");
        printf(SENDER"sleep...\n");
        sleep(5);
    } 
    close(announce_sock);
}

