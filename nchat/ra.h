#ifndef RA_H
#define RA_H

#include <cstdint>
#include <unistd.h>

#include <sys/types.h>

#include <unordered_map>

#include "receiver.h"
#include "announcer.h"

#include "ma.h"
#include "user.h"

struct ra
{
    ra(const uint16_t, const uint16_t);
    void start();
    void cycle();
    ~ra();
    void print_users();
private:
    mac_addr_t mac_addr;
    receiver R;
    void receive_cm();
    announcer A;

    int epollfd;
    fd_set fds;
    int maxfd;

    std::unordered_map<long long, user> users;
};

#endif
