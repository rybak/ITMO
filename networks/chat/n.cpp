#include <unistd.h>
#include <cstdio>

#include "common.h"
#include "kbhit.h"

#include "chat.h"
#include "sl.h"
#include "ports.h"

using namespace std;

void print_usage(char *cmd)
{
    printf("Usage:\n");
    printf("\t%s \n", cmd);
}

const int MSG_KEY = 's';
const int QUIT_KEY = 'q';
const int PRINT_KEY = 'p';
const int HISTORY_KEY = 'l';

void quit()
{
    printf("Quiting ... \n");
    sleep(0);
    exit(0);
}

int main(int argc, char *argv[])
{
    char *udp_port_val = NULL;
    char *tcp_port_val = NULL;
    int index;
    int optch;

    opterr = 0;

    while ((optch = getopt (argc, argv, "u:t:")) != -1)
    {
        switch (optch)
        {
            case 'u':
                udp_port_val = optarg;
                break;
            case 't':
                tcp_port_val = optarg;
                break;
            case '?':
                if (optopt == 'u' || optopt == 't')
                    fprintf (stderr, "Option -%c requires an argument.\n", optopt);
                else if (isprint (optopt))
                    fprintf (stderr, "Unknown option `-%c'.\n", optopt);
                else
                    fprintf (stderr,
                            "Unknown option character `\\x%x'.\n",
                            optopt);
                return 1;
            default:
                abort();
        }
    }
    for (index = optind; index < argc; index++)
    {
        printf ("Non-option argument %s\n", argv[index]);
    }
    uint16_t udp_port = udp_port_val != NULL ? atoi(udp_port_val) : UDP_PORT;
    uint16_t tcp_port = tcp_port_val != NULL ? atoi(tcp_port_val) : TCP_PORT;

    timespec sleep_time;
    sleep_time.tv_sec = 0; /// 0
    sleep_time.tv_nsec = 100l * 1000l * 1000l;
    sl SL(udp_port, tcp_port);
    SL.start();
    for(;;)
    {
        nanosleep(&sleep_time, NULL);
        if (kbhit())
        {
            int ch = getchar();
            switch(ch)
            {
                case MSG_KEY:
                    SL.send_message();
                    break;
                case QUIT_KEY:
                    quit();
                    break;
                case PRINT_KEY:
                    SL.print_users();
                    break;
                case HISTORY_KEY:
                    SL.print_history();
                    break;
                default:
                    break;
            }
        }
        SL.cycle();
    }
}

