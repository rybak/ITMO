#ifndef TCP_SOCKET_H
#define TCP_SOCKET_H
#include <cstdint>
#include <sys/types.h>

int tcp_socket(addrinfo * &, uint16_t);

#endif

