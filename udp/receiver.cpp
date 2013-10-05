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

#define HEADER_FORMAT "%20s\t%20s\t%20s\t%12"
#define RECVR "receiver : "
void print_header()
{
    const char *i = "IP";
    const char *n = "name";
    const char *s = "Student";
    const char *t = "time";
    printf(RECVR"Current time : %ld\n", time(NULL));
    printf(HEADER_FORMAT"s\n", i, s, n, t);

}
void print_entry(const message_t msg)
{
    struct in_addr tmp;
    tmp.s_addr = msg.ip;
    const char *ip_str = inet_ntoa(tmp);
    int len = strlen(ip_str);
    printf(HEADER_FORMAT"lld\n", ip_str,
            msg.student, msg.name, msg.u.timestamp);
}

void print_usage(char *cmd)
{
    printf("Usage:\n");
    printf("\t%s PORT NAME STUDENT\n", cmd);
}

struct server_t
{
    int sock;
    struct sockaddr_in sock_in;
    socklen_t si_len;

    int sender_pid = 0;

    void start(uint16_t port, int s_pid)
    {
        sender_pid = s_pid;
        make_socket(sock, sock_in, port);
        struct timeval tv;
        tv.tv_sec = TIME_INTERVAL;
        tv.tv_usec = 0;
        if (setsockopt(sock,
                    SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv)) < 0)
        {
            die("setsockopt : timeval");
        }
        printf("Port %d\n", htons(sock_in.sin_port));
        si_len = sizeof(sock_in);
    }

    message_t input;
    std::map<int, message_t> clients;
    void cycle()
    {
        receive_message();
        erase_dead_clients();
        print_table();
    }
    int ip = 0;
    void receive_message()
    {
        memset(&input, 0, sizeof(input));
        int msg_len = recvfrom(sock, &input, sizeof(input), 0,
                (struct sockaddr *) &sock_in, &si_len);
        input.name[19] = 0;
        input.student[19] = 0;
        char ip_str[1024];
        inet_ntop(AF_INET, &(sock_in.sin_addr.s_addr), ip_str, sizeof(ip_str));
        int new_ip = sock_in.sin_addr.s_addr;
        printf(RECVR"ip = '%s' == %d\n", ip_str, new_ip);
        if (new_ip != ip && sender_pid != 0)
        {
            FILE *f = fopen("new_ip", "w");
            if (f == NULL)
            {
                die("new_ip FILE");
            }
            fprintf(f, "%d", new_ip);
            fclose(f);
            ip = new_ip;
            // if (kill(sender_pid, SIGCHANGEIP) == -1)
            // {
            //     die("new_ip kill");
            // }
        }
        if (msg_len > 0)
        {
            printf(RECVR"msg_len = %d\n", msg_len);
            clients[input.ip] = input;
            clients[input.ip].u.timestamp = time(NULL);
        } else
        {
            dontdie("recvfrom");
        }
    }
    void erase_dead_clients()
    {
        std::vector<int> to_erase;
        for (auto it = clients.begin();
                it != clients.end(); ++it)
        {
            if (time(NULL) - ((it->second).u.timestamp) > TIME_GAP)
            {
                to_erase.push_back(it->first);
            }
        }
        for (auto it = to_erase.begin();
                it != to_erase.end(); ++it)
        {
            clients.erase(*it);
        }
    }
    void print_table()
    {
        print_header();
        if (clients.empty())
        {
            printf("(NO CLIENTS)\n");
        } else
        {
            for (auto it = clients.begin();
                    it != clients.end(); ++it)
            {
                print_entry(it->second);
            }
        }
        std::cout << std::endl;
    }
    ~server_t()
    {
        close(sock);
    }
} server;


int main(int argc, char* argv[])
{
    if (argc < 3)
    {
        print_usage(argv[0]);
        die(RECVR"Wrong argv");
    }
    int sender_pid = fork();
    if (sender_pid == 0)
    {
        const char *sender_path = "./sender";
        execl(sender_path, sender_path, argv[1], argv[2], argv[3], NULL);
        die("execl");
    }
    server.start(atoi(argv[1]), sender_pid);
    while(true)
    {
        server.cycle();
    }
}

