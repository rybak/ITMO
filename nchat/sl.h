#ifndef SL_H
#define SL_H

#include <netinet/in.h>
#include <unistd.h>

#include <sys/types.h>

#include <unordered_map>
#include <vector>
#include <string>

#include "ports.h"

#include "sender.h"
#include "listener.h"

#include "ma.h"
#include "user.h"

struct sl
{
    sl(const uint16_t, const uint16_t);
    void start();
    void cycle();
    ~sl();
    void send_message();
    void print_users();
private:
    mac_addr_t mac_addr;

    listener L;
    void receive_am();
    sender S;

    fd_set fds;
    int maxfd;

    std::unordered_map<long long, user> users;
    void erase_dead_users();
};

#endif
