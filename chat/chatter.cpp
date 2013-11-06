#include <cstdio>

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

void chatter::read_message()
{
    printf("chatter :: read_message\n");
    s.read_message();
    char ch = getchar();
}
