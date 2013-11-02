#ifndef COMMON_H
#define COMMON_H

#include <signal.h>
#include <stddef.h>

void die(const char *);
void dontdie(const char *);
void make_socket(int &, struct
        sockaddr_in &, uint16_t);
#endif
