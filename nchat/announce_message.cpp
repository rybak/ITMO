#include <endian.h>
#include <ctime>

#include "common.h"
#include "announce_message.h"

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
    timestamp = host_time();
}

announce_message::announce_message()
{
    mac_addr.id = 0;
    get_mac(mac_addr);
}

announce_message::announce_message(const packed_message &msg)
    : timestamp(msg.timestamp)
{
    mac_addr.id = 0;
    mov(mac_addr.ma, msg.ma);
}

void copy_packed_message(packed_message &to, const announce_message &from)
{
    mov(to.ma, from.mac_addr.ma);
    to.timestamp = from.timestamp;
}

void copy_announce_message(announce_message &to, const packed_message &from)
{
    mov(to.mac_addr.ma, from.ma);
    to.timestamp = from.timestamp;
}

