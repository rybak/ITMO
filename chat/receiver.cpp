#include <unistd.h>
#include <ctime>
#include <string>
#include <iostream>

#include <sys/socket.h>
#include <cstdio>
#include <stdlib.h>
#include <cstring>
#include <netdb.h>

#include "chat_message.h"
#include "common.h"
#include "receiver.h"

receiver::receiver(const uint16_t port)
    : port(port)
{
    printf("receiver contructor\n");
    struct addrinfo *servinfo;
    struct addrinfo hints;
    int status;
    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_INET; // don’t care IPv4 or IPv6
    hints.ai_socktype = SOCK_STREAM; // TCP stream sockets
    hints.ai_flags = AI_PASSIVE | AI_ALL; // fill in my IP for me
    char service[10];
    sprintf(service, "%d", port);
    if ((status = getaddrinfo(NULL, service, &hints, &servinfo)) != 0)
    {
        switch(status)
        {
            case EAI_ADDRFAMILY:
                die("getaddrinfo ADDRFAM");
            case EAI_AGAIN:
                die("getaddrinfo AGAIN");
            case EAI_BADFLAGS:
                die("getaddrinfo BADFLAGS");
            case EAI_FAIL:
                die("getaddrinfo FAIL");
            default:
                printf("ERROR CODE = %d\n", status);
                die("");
        }
    }
    sock = socket(servinfo->ai_family, servinfo->ai_socktype,
            servinfo->ai_protocol);
    if (sock < 0)
    {
        die("receiver constructor : socket");
    }
    int yes = 1;
    if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) < 0)
    {
        die("receiver constructor : setsockopt");
    }
    if (bind(sock, servinfo->ai_addr, servinfo->ai_addrlen) < 0)
    {
        die("receiver constructor : bind");
    }
    if (listen(sock, 10) < 0)
    {
        die("receiver constructor : listen");
    }
    freeaddrinfo(servinfo);
}

receiver::~receiver()
{
    close(sock);
}

chat_message receiver::receive_message()
{
    int conn_sock = accept(sock, NULL, NULL);
    if (conn_sock < 0)
    {
        die("receiver::receive_message : accept");
    }
    int cnt = 0;
    cm_header h;
    cnt = recvfrom(conn_sock, &h, sizeof(h), 0, NULL, NULL);
    if (cnt < 0)
    {
        die("receiver::receive_message : recvfrom header");
    }
    chat_message msg(h);
    msg.to_host();
    if (msg.len > 10000 || msg.len < 0)
    {
        msg.to_host();
    }
    if (msg.len > 10000 || msg.len < 0)
    {
        msg.text = "bad len";
        std::cout << "len = " << msg.len << std::endl;
        return msg;
    }
    int len = msg.len;
    std::cout << 1 << std::endl;
    std::cout << "size " << len << std::endl;
    char *buffer = new char[len];
    std::cout << 1.5 << std::endl;
    cnt = recvfrom(conn_sock, buffer, len, 0, NULL, NULL);
    if (cnt < 0)
    {
        die("receiver::receive_message : recvfrom message");
    }
    std::cout << 2 << std::endl;
    msg.text = std::string(buffer);
    std::cout << 3 << std::endl;
    delete[] buffer;
    return msg;
}

