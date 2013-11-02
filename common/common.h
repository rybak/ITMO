#ifndef COMMON_H
#define COMMON_H

#include <signal.h>
#include <stdint.h>

void die(const char *);
void dontdie(const char *);
void make_udp_socket(int &, struct
        sockaddr_in &, uint16_t);
#endif
