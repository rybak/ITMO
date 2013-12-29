#include <cstdio>
#include <cstring>
#include <sys/types.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#include <iostream>

#include <ctime>

#include "common.h"

void die(const char *s)
{
    perror(s);
    exit(1);
}

void dontdie(const char *s)
{
    perror(s);
    printf("\n");
}

long long host_time()
{
    long long res = time(NULL) * 1000ll;
    return res;
}

std::string ip_string(const int ip)
{
    char ip_str[1024];
    inet_ntop(AF_INET, &ip, ip_str, sizeof(ip_str));
    return std::string(ip_str);
}

std::string time_string(const long long t)
{
    static const size_t TIME_BUF_LEN = 100;
    static const char *TIME_FORMAT = "%Y:%m:%d %H:%M:%S";
    char time_buf[TIME_BUF_LEN];
    time_t timestamp = t / 1000l;
    tm *tm_info = localtime(&timestamp);
    strftime(time_buf, TIME_BUF_LEN, TIME_FORMAT, tm_info);
    return std::string(time_buf);
}

