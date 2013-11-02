#include <endian.h>
#include <ctime>

#include "message.h"
#include "chat.h"
#include "common.h"

void announce_message::to_net()
{
    timestamp = htobe64(timestamp);
}

void announce_message::to_host()
{
    timestamp = be64toh(timestamp);
}

void announce_message::update()
{
    timestamp = time(NULL);
}

announce_message::announce_message()
{
    get_mac(mac_addr);
}


