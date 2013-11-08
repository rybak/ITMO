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

announce_message::announce_message(const packed_message &msg)
    : timestamp(msg.timestamp)
{
    mov(mac_addr.ma, msg.ma);
}

void copy_a_msg(packed_message &to, const announce_message &from)
{
    mov(to.ma, from.mac_addr.ma);
    to.timestamp = from.timestamp;
}

void copy_announce_message(announce_message &to, const packed_message &from)
{
    mov(to.mac_addr.ma, from.ma);
    to.timestamp = from.timestamp;
}
