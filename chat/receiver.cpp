#include <unistd.h>
#include <ctime>
#include <string>

// future includes : 
#include <sys/socket.h>
#include <cstdio>

#include "receiver.h"

receiver::receiver(const uint16_t port)
    : port(port)
{
    printf("receiver contructor\n");
}

receiver::~receiver()
{
    close(sock);
}

void receiver::receive_message()
{
    // accept TODO
    char *buffer = new char[1024];
    int cnt = 0;
    // read TODO
    if (cnt < 0)
    {
        // error 
    }
    delete[] buffer;
}

