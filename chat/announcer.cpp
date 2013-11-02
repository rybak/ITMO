#include <ctime>
#include "chat.h"

#include "announcer.h"

void announcer::announce()
{
    if (time(NULL) - last_announce_time < TIME_INTERVAL)
    {

    }
}



