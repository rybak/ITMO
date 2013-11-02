#include <cstdint>
#include <cstdio>

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

