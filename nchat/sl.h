#ifndef SL_H
#define SL_H

#include <cstdint>
#include <utility>
#include <string>
#include <vector>
#include <unordered_map>


#include "ma.h"
#include "user.h"

#include "sender.h"
#include "listener.h"

struct sl
{
    sl(const uint16_t, const uint16_t);
    void start();
    void cycle();
    ~sl();
    void send_message();
    void print_users();
    void print_history();
private:
    mac_addr_t mac_addr;

    uint16_t tcp_port;
    void send_message_to_user(user &, const std::string &, long long);
    listener L;
    void receive_am();
    void revive(user &, long long);

    fd_set fds;
    int maxfd;

    std::vector<std::pair<long long, std::string> > history;
    void save_message(const std::string &);

    std::unordered_map<long long, user> users;
    void mark_dead_users();
};

#endif

