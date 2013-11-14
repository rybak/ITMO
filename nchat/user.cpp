#include <ctime>

#include "announce_message.h"
#include "common.h"

#include "user.h"

user::user(const announce_message &amsg)
    : mac_addr(amsg.mac_addr),
    timestamp(amsg.timestamp),
    ip(amsg.ip), dead(false)
{
    this->update();
}

void user::update()
{
    long long ht = host_time();
    offset = timestamp - ht;
}

