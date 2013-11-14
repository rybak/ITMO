#include <endian.h>
#include <arpa/inet.h>
#include <ctime>

#include "ma.h"
#include "chat_message.h"
#include "common.h"

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

chat_message::chat_message(const std::string &text)
    : timestamp(host_time()), text(text), len(text.length())
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

void make_header(cm_header &h, const chat_message &msg)
{
    h.timestamp = msg.timestamp;
    h.len = msg.len;
    mov(h.ma, msg.mac_addr.ma);
}

void chat_message::to_user_time(long long offset)
{
    timestamp += offset;
}

