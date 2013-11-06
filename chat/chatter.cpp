#include <ctime>
#include <cstring>
#include "chat.h"
#include "chatter.h"
#include "common.h"

void chatter::announce()
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
                (struct sockaddr *) &aa, sizeof(aa)) < 0)
        {
            dontdie("announce :: sendto");
        }
    }
}

bool chatter::good_timing()
{
    return time(NULL) - last_announce_time < TIME_INTERVAL;
}

chatter::chatter(const uint16_t port)
{
    aa.sin_addr.s_addr = htonl(-1);
    aa.sin_port = htons(port);
    int yes = 1;
    if (setsockopt(announce_sock, SOL_SOCKET,
            SO_BROADCAST, &yes, sizeof(int)) < 0)
    {
        die("setsockopt");
    }
}

chatter::~chatter()
{
    close(announce_sock);
}

