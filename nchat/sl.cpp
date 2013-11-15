#include <cstring>
#include <iostream>

#include "chat.h"
#include "common.h"
#include "announce_message.h"
#include "chat_message.h"
#include "user.h"
#include "sl.h"

sl::sl(const uint16_t udp_port, const uint16_t tcp_port)
    : S(tcp_port), L(udp_port)
{
    get_mac(mac_addr);
    print_mac(mac_addr);
    std::cout << std::endl;
}

sl::~sl()
{
}

void sl::start()
{
    FD_ZERO(&fds);
    FD_SET(L.sock, &fds);
    maxfd = L.sock;
}

void sl::cycle()
{
    mark_dead_users();
    fd_set read_fds;
    FD_ZERO(&read_fds);
    memcpy(&read_fds, &fds, sizeof(fds));
    timeval tv = {0, 100000};
    int nready = select(maxfd + 1, &read_fds, NULL, NULL, &tv);
    if (nready == -1)
    {
        die("sl::cycle : select");
    }
    for(int i = 0; i <= maxfd && nready > 0; i++)
    {
        if (FD_ISSET(i, &read_fds))
        {
            nready--;
            if (i == L.sock)
            {
                receive_am();
            }
        }
    }
}

void sl::receive_am()
{
    announce_message msg = L.receive_message();
    std::string ip_str = ip_string(msg.ip);
    print_mac(msg.mac_addr);
    std::cout << ' ' << ip_str;
    std::cout << ' ' << time_string(msg.timestamp) << std::endl;
    long long id = msg.mac_addr.id;
    if (users.count(id) > 0)
    {
        users.at(id).dead = false;
        users.at(id).timestamp = msg.timestamp;
        users.at(id).update();
    } else
    {
        users.emplace(id, user(msg));
    }
}

namespace
{
    std::string read_message()
    {
        std::cout << "Enter message :" << std::endl;
        std::string msg;
        std::getline(std::cin, msg);
        return msg;
    }
}

void sl::save_message(const std::string &text)
{
    history.push_back(make_pair(host_time(), text));
}

void sl::print_history()
{
    for (auto it = history.begin(); it != history.end(); ++it)
    {
        std::cout << "[" << time_string((*it).first) << "] "
            << (*it).second << std::endl;
    }
}

void sl::send_message()
{
    std::string text = read_message();
    if (text.length() == 0)
    {
        return;
    }
    save_message(text);
    std::cout << "message to send : " << text << std::endl;
    for (auto it = users.begin(); it != users.end(); ++it)
    {
        if (it->second.dead)
        {
            continue;
        }
        std::cout << "sending to ";
        print_mac(it->second.mac_addr);
        std::cout << std::endl;
        S.send_message(it->second, text);
    }
}

namespace
{
    void print_header()
    {
        printf("Current time : %ld\n", time(NULL));
        printf("%10s%18s%10s\n", "time", "mac", "ip (%%d)");
    }

    void print_entry(const user &u)
    {
        printf("%10lld ", u.timestamp / 1000);
        print_mac(u.mac_addr);
        printf(" %d ", u.ip);
        if (u.dead)
        {
            std::cout << "[dead]" << std::endl;
        }
    }
}

void sl::print_users()
{
    print_header();
    if (users.empty())
    {
        printf("(NO users)\n");
    } else
    {
        for (auto it = users.begin(); it != users.end(); ++it)
        {
            print_entry(it->second);
        }
    }
    std::cout << std::endl;
}

bool is_dead_user(const user &u)
{
    long long ht = host_time();
    return (ht - u.timestamp) / 1000l > TIME_GAP;
}

void sl::mark_dead_users()
{
    for (auto it = users.begin(); it != users.end(); ++it)
    {
        if (is_dead_user(it->second))
        {
            it->second.dead = true;
        }
    }
}

