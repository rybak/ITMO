#include <ctime>

#include "message.h"

#include "user.h"

user::user(const announce_message &amsg)
    : mac_addr(amsg.mac_addr),
    timestamp(amsg.timestamp),
    ip(amsg.ip)
{
    this->update();
}

void user::update()
{
    long long ht = time(NULL) * 1000l;
    offset = timestamp - ht;
}

