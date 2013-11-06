#include <ctime>
#include <cstring>

// future includes : 
#include <cstdio>
// /future

#include "receiver.h"

{
    struct chat_user
    {
        mac_addr_t ma;
        string nick;


    };
}
receiver::receiver()
{
    // init tcp listen sock
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
    // accept TODO
    char *buffer = new char[1024];
    // read TODO
    int cnt;
    add_message(buffer, cnt);
    delete[] buffer;
}

void receiver::print_messages()
{

}

