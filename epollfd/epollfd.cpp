#include <sys/epoll.h>
#include <unistd.h>

#include <stdexcept>
#include <vector>
#include <iostream>
#include <assert.h>

#include "epollfd.h"

epollfd::epollfd(int max_events)
    : epfd(epoll_create(42))
{
    if (epfd < 0)
    {
        throw std::runtime_error("epoll_create");
    }
}

epollfd::~epollfd()
{
    if (close(epfd))
    {
        throw std::runtime_error("epollfd: close");
    }
}

void epollfd::cycle()
{
    for (auto curr : sub_tasks)
    {
        sub(curr.second);
    }
    sub_tasks.clear();
    for (auto curr : unsub_tasks)
    {
        unsub(curr);
    }
    unsub_tasks.clear();

    std::vector<epoll_event> tmp(max_events);
    int nfds = epoll_wait(epfd, tmp.data(), max_events, -1);
    if (nfds < 0)
    {
        throw std::runtime_error("epoll_wait");
    }
    for (int i = 0; i < nfds; ++i)
    {
        epoll_event &curr = tmp[i];
        if (curr.events & EPOLLERR)
        {
            for (auto ev_action : actions.at(curr.data.fd))
            {
                // 1 second for map
                // 2 second for cont_err
                ev_action.second.second();
            }
            continue;
        }
        for (auto ev : {EPOLLIN, EPOLLOUT})
        {
            if (curr.events & ev)
            {
                assert(actions.at(static_cast<int>(curr.data.fd)).
                        count(ev));
                actions[static_cast<int>(curr.data.fd)][ev].first();
            }
        }
    }
}

void epollfd::subscribe(int fd, uint32_t ev,
        cont_t cont, cont_t cont_err)
{
    uint32_t &curr_ee = events[fd];
    if ((curr_ee & ev) || sub_tasks.count({fd, ev}))
    {
        throw std::runtime_error("subscribe : sub already exist");
    }
    sub_tasks[{fd, ev}] = {fd, ev, cont, cont_err};
}

void epollfd::unsubscribe(int fd, uint32_t ev)
{
    if (sub_tasks.count({fd, ev}))
    {
        sub_tasks.erase({fd, ev});
        return;
    }
    uint32_t &curr_events = events[fd];
    if (((curr_events & ev) != ev) || unsub_tasks.count({fd, ev}))
    {
        throw std::runtime_error("unsubscribe : doesn't exist");
    }
    unsub_tasks.insert({fd, ev});
}

void epollfd::sub(const sub_task &task)
{
    std::cerr << "sub fd == " << task.fd << std::endl;
    uint32_t curr_events = events[task.fd];
    uint32_t new_events = curr_events | task.events;
    int op = task.events == new_events ?
        EPOLL_CTL_ADD : EPOLL_CTL_MOD;
    struct epoll_event ee;
    ee.events = new_events;
    ee.data.fd = task.fd;
    if (epoll_ctl(epfd, op, task.fd, &ee))
    {
        perror("sub");
        throw std::runtime_error("sub : epoll_ctl");
    }
    actions[task.fd][task.events] = {task.cont, task.cont_err};
    events[task.fd] = new_events;
}

void epollfd::unsub(const fdev &task)
{
    std::cerr << "unsub fd == " << task.first << std::endl;
    int fd = task.first;
    uint32_t ev = task.second;
    uint32_t curr_events = events[fd];
    uint32_t new_events = curr_events ^ ev;
    int op = new_events == 0 ? EPOLL_CTL_DEL : EPOLL_CTL_MOD;
    struct epoll_event ee;
    ee.events = new_events;
    ee.data.fd = fd;
    if (epoll_ctl(epfd, op, fd, &ee))
    {
        perror("unsub");
        throw std::runtime_error("unsub : epoll_ctl");
    }
    actions.at(fd).erase(ev);
    if (actions.at(fd).empty())
    {
        actions.erase(fd);
    }
    events[fd] = new_events;
}

