#include <cstdlib>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <cstring>

#include <stdexcept>
#include <iostream>
#include <utility>
#include <string>
#include <sys/epoll.h>

#include "chat.h"
#include "common.h"
#include "announce_message.h"
#include "chat_message.h"
#include "ra.h"

ra::ra(const uint16_t udp_port, const uint16_t tcp_port)
    : R(tcp_port), A(udp_port)
{
    get_mac(mac_addr);
    std::cout << "host mac = ";
    print_mac(mac_addr);
    std::cout << std::endl;
    epollfd = epoll_create1(0);
    if (epollfd < 0)
    {
        die("sl::sl : epoll_create1");
    }
    epoll_event ev;
    ev.events = EPOLLIN | EPOLLOUT | EPOLLERR;
    ev.data.fd = R.sock;
    if (epoll_ctl(epollfd, EPOLL_CTL_ADD, R.sock, &ev) == -1)
    {
        die("sl::sl : epoll_ctl add R.sock");
    }
}

ra::~ra()
{
    close(epollfd);
}

void ra::start()
{
}

void ra::cycle()
{
    const size_t MAX_EVENTS = 20;
    A.announce();
    epoll_event events[MAX_EVENTS];
    int cnt = epoll_wait(epollfd, events, MAX_EVENTS, 1);
    if (cnt < 0)
    {
        die("ra::cycle : epoll_wait");
    }
    for (int i = 0; i < cnt; ++i)
    {
        if (events[i].data.fd == R.sock)
        {
            receive_cm();
        }
    }
}

namespace
{
    void print_time(chat_time_t t)
    {
        std::cout << " [" << time_string(t) << "] ";
    }

    void cls()
    {
        std::cout << "\033[2J\033[1;1H";
    }

    void print_chat_message(const chat_message &msg)
    {
        print_mac(msg.mac_addr);
        print_time(msg.timestamp);
        std::cout << " : (" << msg.len << ") " << msg.text << std::endl;
    }
}

void ra::receive_cm()
{
    try
    {
        chat_message msg = R.receive_message();
        save_message(msg);
        if (msg.timestamp < last_timestamp())
        {
            print_history();
        } else
        {
            print_chat_message(msg);
        }
    }
    catch (std::runtime_error &e)
    {
        perror("receiver::receive_message");
        std::cerr << e.what() << std::endl;
    }
}

void ra::save_message(const chat_message &msg)
{
    if (history.count(msg.timestamp) == 0)
    {
        history.insert(std::make_pair(msg.timestamp, std::vector<chat_message>()));
    }
    history.at(msg.timestamp).push_back(msg);
}

chat_time_t ra::last_timestamp()
{
    if (history.empty())
    {
        return 0;
    }
    if (history.end()->second.empty())
    {
        die("history assert");
    }
    auto it = history.end();
    --it;
    return it->second.begin()->timestamp;
}

void ra::print_history()
{
    std::cout << "Received messages : " << std::endl;
    for(auto &v : history)
    {
        for (auto &p : v.second)
        {
            print_chat_message(p);
        }
    }
}

