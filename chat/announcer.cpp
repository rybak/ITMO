#include <ctime>

#include <cstring>
#include <cstdio>

#include "common.h"
#include "chat.h"
#include "announce_message.h"

#include "announcer.h"

void announcer::announce()
{
    if (good_timing())
    {
        printf("announcer::announce\n");
        msg.update();
        announce_message net_msg(msg);
        net_msg.to_net();
        packed_message pmsg;
        copy_packed_message(pmsg, net_msg);
        const size_t msg_size = sizeof(pmsg);
        last_announce_time = time(NULL);
        if (sendto(sock, &pmsg, msg_size, 0,
                (struct sockaddr *) &aa, sizeof(aa)) < 0)
        {
            dontdie("announce::sendto");
        }
    }
}

bool announcer::good_timing()
{
    return time(NULL) - last_announce_time > TIME_INTERVAL;
}

announcer::announcer(const uint16_t port)
    : last_announce_time(time(NULL) - TIME_INTERVAL)
{
    printf("announcer contructor port = %d\n", (int) port);
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

