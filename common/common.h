#ifndef COMMON_H
#define COMMON_H

#include <signal.h>
#include <stdint.h>
#include <string>

void die(const char *);
void dontdie(const char *);
void make_udp_socket(int &, struct
        sockaddr_in &, uint16_t);
long long host_time();
std::string ip_string(const int);
std::string time_string(const long long);

#endif
