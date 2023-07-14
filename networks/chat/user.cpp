#include <ctime>

#include "common.h"

#include "user.h"

user::user(const announce_message &amsg)
    : mac_addr(amsg.mac_addr),
    timestamp(amsg.timestamp),
    ip(amsg.ip),
    dead(false),
    message_sent(0)
{
    this->update();
}

void user::update()
{
    long long ht = host_time();
    offset = timestamp - ht;
}

