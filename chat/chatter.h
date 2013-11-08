#ifndef CHATTER_H
#define CHATTER_H

#include <netinet/in.h>
#include <unistd.h>

#include <sys/types.h>

#include <unordered_map>
#include <vector>
#include <string>

#include "ports.h"

#include "sender.h"
#include "receiver.h"
#include "announcer.h"
#include "listener.h"

#include "ma.h"
#include "user.h"

struct chatter
{
    chatter(const uint16_t, const uint16_t);
    void start();
    void cycle();
    ~chatter();
    void read_message();
    void print_users();
private:
    mac_addr_t mac_addr;

    listener L;
    void receive_am();
    receiver R;
    void receive_cm();
    sender S;
    announcer A;

    fd_set fds;
    int maxfd;
   // std::map<long long, std::string> users;
    
    std::unordered_map<long long, user> users;
    void erase_dead_users();
};

#endif
