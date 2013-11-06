#include <cstdint>
#include <cstdio>

#include "common.h"
#include "announcer.h"
#include "chat.h"
#include "ma.h"
#include "kbhit.h"

#include "chatter.h"

using namespace std;

const uint16_t PORT = 1235;

void print_usage(char *cmd)
{
    printf("Usage:\n");
    printf("\t%s \n", cmd);
}

const char MSG_KEY = 's';

int main(int argc, char *argv[])
{
    mac_addr_t ma;
    get_mac(ma);
    print_mac(ma);
    announcer a;
    chatter c;

    for(;;)
    {
        a.announce();
        timespec sleep_time;
        sleep_time.tv_sec = 0;
        sleep_time.tv_nsec = 100l * 1000l * 1000l;
        nanosleep(&sleep_time, NULL);
        if (kbhit())
        {
            char ch = getchar();
            printf("Key %d = '%c' is pressed\n", (int) ch, ch);
            switch(ch)
            {
                MSG_KEY:
                    c.read_message();
                

                default:
                    break;
            }
        }
        c.cycle();
    } 
}

