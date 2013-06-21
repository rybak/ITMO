#include <stddef.h>
#include <stdlib.h>
#include <errno.h>

#include <string.h>
#include <sys/types.h>
#include <limits.h>

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include <sys/socket.h>
#include <netdb.h>
#include <poll.h>
#include <sys/select.h>
#include <sys/wait.h>

#include <iostream>

#include "http.h"

#define ERROR "Error : sigd : "

pollfd poll_sock;
int listen_sock;

const size_t MAX_EVENTS = 10;
void init(char *argv[])
{
    listen_sock = init_listen_socket(argv[1], argv[2]);
}

int main(int argc, char *argv[])
{
    int dpid = fork();
    if (dpid != 0)
    {
        waitpid(dpid, NULL, 0);
        exit(0);
    }
    int sid = setsid();
    if (sid < 0)
    {
        perror(ERROR"creating session ");
        exit(1);
    }
    std::cerr << "sigd server sid == " << sid << std::endl;
    init(argv);
    while (true)
    {
        std::cerr << "accept" << std::endl;
        int conn_sock = accept(listen_sock, NULL, NULL);
        if (conn_sock < 0)
        {
            perror(ERROR"accept");
            continue;
        }
        int spid = fork();
        if (spid != 0)
        {
            std::cerr << "\tspid == " << spid << std::endl;
            close(conn_sock);
            waitpid(spid, NULL, 0);
        } else
        {
            close(listen_sock);
            process_client(conn_sock);
        }
    }
    exit(EXIT_SUCCESS);
}

