#ifndef CHAT_H
#define CHAT_H 1

#define CHAT_STR "chat : "

#include <endian.h>
#include <cstdlib>
#include <ctime>
#include "common.h"
#include "ma.h"

const size_t TIME_INTERVAL = 5;
const size_t TIME_GAP = 15;

inline long long net_time()
{
    long long res = time(NULL) * 1000 + (rand() % 1000);
    res = htobe64(res);
    return res;
}

#endif
