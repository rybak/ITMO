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
#include "announcer.h"
#include "chat.h"
#include "ma.h"

using namespace std;

const uint16_t PORT = 1235;

void print_usage(char *cmd)
{
    printf("Usage:\n");
    printf("\t%s \n", cmd);
}

void print_mac(const mac_addr_t &ma)
{
    printf("%c%c%c%c%c%c", ma[0], ma[1], ma[2], ma[3], ma[4], ma[5]);
}

int main(int argc, char *argv[])
{
    mac_addr_t ma;
    get_mac(ma);
    print_mac(ma);
    exit(0);
    announcer a;

    for(;;)
    {
        a.announce();
        sleep(1);
    } 
}

