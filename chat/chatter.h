#ifndef CHATTER_H
#define CHATTER_H

#include <netinet/in.h>
#include <unistd.h>

#include <sys/types.h>

#include <map>
#include <vector>
#include <string>

#include "message.h"
#include "sender.h"
#include "receiver.h"
#include "ports.h"
#include "announcer.h"
#include "listener.h"
#include "ma.h"

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

    receiver R;
    sender S;
    listener L;
    announcer A;

    fd_set fds;
    int maxfd;
   // std::map<long long, std::string> users;
    
    std::map<long long, announce_message> users;
    void erase_dead_users();
};

#endif
