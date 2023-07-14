#include <sys/socket.h>
#include <netdb.h>
#include <cstdio>

#include "common.h"

#include "tcp_socket.h"

int tcp_socket(addrinfo * &servinfo, uint16_t port)
{
    int status;
    struct addrinfo hints = {AI_PASSIVE | AI_ALL, AF_INET, SOCK_STREAM, 0, 0, 0, 0, 0};
    char service[10];
    sprintf(service, "%d", port);
    if ((status = getaddrinfo(NULL, service, &hints, &servinfo)) != 0)
    {
        die("getaddrinfo");
    }
    return socket(servinfo->ai_family, servinfo->ai_socktype, servinfo->ai_protocol);
}

