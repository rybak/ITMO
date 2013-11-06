#include "chat.h"
#include "chatter.h"
#include "common.h"

chatter::chatter(const uint16_t port)
{
}
chatter::~chatter()
{
}

void chatter::cycle()
{
    s.cycle();
    r.cycle();
}

