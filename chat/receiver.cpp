#include <ctime>
#include <cstring>

// future includes : 
#include <cstdio>
// /future

#include "receiver.h"

receiver::receiver()
{
}

receiver::~receiver()
{
    close(listen_sock);
}

void receiver::cycle()
{
    receive_messages();
    print_messages();
}

void receiver::receive_messages()
{
}

void receiver::print_messages()
{

}
