#include <cstdint>
#include <cstring>
#include <cstdio>

#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <netinet/in.h>
#include <ctime>
#include <iostream>

#include "chat_message.h"
#include "common.h"
#include "sender.h"

sender::sender(uint16_t port)
    : port(port)
{
    printf("sender contructor\n");
}

sender::~sender()
{
    close(sock);
}

void sender::send_message(const user &u, const std::string &text)
{
    std::cout << 1 << std::endl;
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
        die("getaddrinfo");
    }
    int sock = socket(servinfo->ai_family,
            servinfo->ai_socktype, servinfo->ai_protocol);
    if (sock < 0)
    {
        std::cerr << "sock == " << sock << std::endl;
        die("sender::send_message : socket");
    }
    freeaddrinfo(servinfo);
    sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = htonl(u.ip);
    std::cout << 1.5 << std::endl;
    int yes = 0;
    if (setsockopt(sock, SOL_SOCKET,
                SO_REUSEADDR, &yes, sizeof(int)) < 0)
    {
        die("setsockopt");
    }
    std::cout << 1.7 << std::endl;
    if ((status = connect(sock, (sockaddr *) &addr, sizeof(addr))) < 0)
    {
        std::cerr << "status == " << status << std::endl;
        die("sender::send_message : connect");
    }
    std::cout << 2 << std::endl;
    chat_message msg;
    msg.timestamp = host_time();
    msg.text = "test 42 ";
    msg.len = text.length();
    msg.mac_addr.id = u.mac_addr.id;
    msg.to_net();

    std::cout << 3 << std::endl;
    cm_header h;
    h.timestamp = msg.timestamp;
    h.len = msg.len;
    mov(h.ma, msg.mac_addr.ma);

    std::cout << 4 << std::endl;
    int cnt;
    socklen_t si_len = sizeof(addr);
    cnt = sendto(sock, &h, sizeof(h), 0, (struct sockaddr *) &addr, si_len);
    if (cnt < 0)
    {
        printf("send to error");
    }

    std::cout << 5 << std::endl;
    cnt = sendto(sock, msg.text.c_str(), msg.len, 0, (struct sockaddr *) &addr, si_len);
    if (cnt < 0)
    {
        printf("send to error");
    }
    std::cout << 6 << std::endl;
    close(sock);
}

