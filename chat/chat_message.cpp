#include <endian.h>
#include <arpa/inet.h>
#include <ctime>

#include "ma.h"
#include "chat_message.h"

void chat_message::to_net()
{
    timestamp = htobe64(timestamp);
    len = htobe32(len);
}

void chat_message::to_host()
{
    timestamp = be64toh(timestamp);
    len = be32toh(len);
}

chat_message::chat_message()
{
    mac_addr.id = 0;
    get_mac(mac_addr);
}

chat_message::chat_message(const cm_header &h)
    : timestamp(h.timestamp), len(h.len)
{
    mac_addr.id = 0;
    mov(mac_addr.ma, h.ma);
}

