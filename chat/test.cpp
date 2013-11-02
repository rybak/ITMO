#include <sys/ioctl.h>
#include <net/if.h> 
#include <unistd.h>
#include <netinet/in.h>
#include <string.h>
#include <cstdlib>

#include <cstdio>

#include "../common/common.h"

typedef unsigned char mac_addr_t[6];

void print_mac(const mac_addr_t &ma)
{
    printf("%02x:%02x:%02x:%02x:%02x:%02x\n", ma[0], ma[1], ma[2], ma[3], ma[4], ma[5]);
}

int main(int argc, char *argv[])
{
    mac_addr_t ma;
    get_mac(ma);
    print_mac(ma);
    exit(0);
}

