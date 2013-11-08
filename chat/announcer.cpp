#include <ctime>

#include <cstring>
#include <cstdio>

#include "chat.h"
#include "announcer.h"
#include "common.h"

void announcer::announce()
{
    if (good_timing())
    {
        msg.update();
        announce_message net_msg(msg);
        net_msg.to_net();
        const size_t msg_size = sizeof(long long) + sizeof(_mac_addr_t);
        char buf[msg_size];
        memcpy(buf, &(net_msg.mac_addr.ma), sizeof(_mac_addr_t));
        memcpy(buf + sizeof(_mac_addr_t), &(net_msg.timestamp), sizeof(long long));
        if (sendto(sock, buf, msg_size, 0,
                (struct sockaddr *) &aa, sizeof(aa)) < 0)
        {
            dontdie("announce :: sendto");
        }
    }
}

bool announcer::good_timing()
{
    return time(NULL) - last_announce_time < TIME_INTERVAL;
}

announcer::announcer(const uint16_t port)
{
    printf("announcer contructor\n");
    make_udp_socket(sock, aa, 0);
    aa.sin_addr.s_addr = htonl(-1);
    aa.sin_port = htons(port);
    int yes = 1;
    if (setsockopt(sock, SOL_SOCKET,
            SO_BROADCAST, &yes, sizeof(int)) < 0)
    {
        die("setsockopt");
    }
}

announcer::~announcer()
{
    close(sock);
}

