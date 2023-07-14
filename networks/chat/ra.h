#ifndef RA_H
#define RA_H

#include <cstdint>
#include <unordered_map>
#include <map>
#include <vector>

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
    void print_history();
private:
    mac_addr_t mac_addr;
    receiver R;
    void receive_cm();
    void save_message(const chat_message &);
    std::map<chat_time_t, std::vector<chat_message> > history;
    chat_time_t last_timestamp();

    announcer A;

    int epollfd;
    fd_set fds;
    int maxfd;

    std::unordered_map<long long, user> users;
};

#endif

