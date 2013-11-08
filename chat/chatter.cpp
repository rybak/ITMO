#include <cstdlib>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <cstring>

#include <cstdio>
#include <iostream>
#include <map>
#include <vector>

#include "chat.h"
#include "chatter.h"
#include "common.h"
#include "ma.h"

chatter::chatter(const uint16_t udp_port, const uint16_t tcp_port)
    : S(tcp_port), R(tcp_port),
    A(udp_port), L(udp_port)
{
    get_mac(mac_addr);
    print_mac(mac_addr);
    printf("\n");
}

chatter::~chatter()
{
}


void chatter::start()
{
    FD_ZERO(&fds);
//    FD_SET(R.sock, &fds);
    FD_SET(L.sock, &fds);
    maxfd = L.sock;
}

void chatter::cycle()
{
    A.announce();
    //    S.cycle();
    //    R.cycle();
    //     erase_dead_users();
    printf("chatter :: cycle\n");
    fd_set read_fds;
    FD_ZERO(&read_fds);
    memcpy(&read_fds, &fds, sizeof(fds));
    timeval tv = {0, 100000};
    int nready = select(maxfd + 1, &read_fds, NULL, NULL, &tv);
    printf("nready = %d\n", nready);
    if (nready == -1)
    {
        return;
    }
    for(int i = 0; i <= maxfd && nready > 0; i++)
    {
        if (FD_ISSET(i, &read_fds))
        {
            nready--;
            if (i == L.sock)
            {
                announce_message msg = L.receive_message();
            }
            else
            {
                // R.sock
            }
        }

    }


}

void chatter::read_message()
{
    printf("chatter :: read_message\n");
    S.read_message();
    char ch = getchar();
}

namespace
{
    void print_header()
    {
        printf("Current time : %ld\n", time(NULL));
        printf("%10s%10s\n", "time", "mac");
    }

    void print_entry(const announce_message msg)
    {
        printf("%10lld ", msg.timestamp);
        print_mac(msg.mac_addr);
        printf("\n");
    }
}

void chatter::print_users()
{
    print_header();
    if (users.empty())
    {
        printf("(NO users)\n");
    } else
    {
        for (auto it = users.begin();
                it != users.end(); ++it)
        {
            print_entry(it->second);
        }
    }
    std::cout << std::endl;
}

void chatter::erase_dead_users()
{
    std::vector<long long> to_erase;
    for (auto it = users.begin();
            it != users.end(); ++it)
    {
        if (time(NULL) - ((it->second).timestamp) > TIME_GAP)
        {
            to_erase.push_back(it->first);
        }
    }
    for (auto it = to_erase.begin();
            it != to_erase.end(); ++it)
    {
        users.erase(*it);
    }
}


