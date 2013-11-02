#include <ctime>
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
        const size_t msg_size = sizeof(net_msg);
        char buf[msg_size];
        memcpy(buf, &net_msg, msg_size);
        if (sendto(announce_sock, buf, msg_size, 0,
                (struct sockaddr *) &announce_addr, sizeof(announce_addr)) < 0)
        {
            dontdie("announce :: sendto");
        }
    }
}

bool announcer::good_timing()
{
    return time(NULL) - last_announce_time < TIME_INTERVAL;
}

announcer::announcer()
{
    announce_addr.sin_addr.s_addr = htonl(-1);
    announce_addr.sin_port = htons(ANNOUNCE_PORT);
    int yes = 1;
    if (setsockopt(announce_sock, SOL_SOCKET,
            SO_BROADCAST, &yes, sizeof(int)) < 0)
    {
        die("setsockopt");
    }
}

announcer::~announcer()
{
    close(announce_sock);
}
