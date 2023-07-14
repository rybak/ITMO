#ifndef COMMON_H
#define COMMON_H

#include <cstdint>
#include <string>

void die(const char *);
void dontdie(const char *);
long long host_time();
std::string ip_string(const int);
std::string time_string(const long long);

#endif
