#include <unistd.h>
#include <ctime>
#include <string>

// future includes : 
#include <sys/socket.h>
#include <cstdio>

// /future

#include "receiver.h"
#include "ma.h"
namespace {
    struct chat_user
    {
        mac_addr_t ma;
        std::string nick;


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
    int cnt = 0;
    // read TODO
    if (cnt < 0)
    {
        // error 
    }
    add_message(buffer, (size_t) cnt);
    delete[] buffer;
}

void receiver::print_messages()
{

}

void receiver::add_message(char *buf, size_t buflen)
{
}
