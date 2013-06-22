#include <stdexcept>
#include <iostream>

#include <sys/epoll.h>
#include <unistd.h>
#include "epollfd.h"

epollfd::epollfd()
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
    for (auto it = sub_tasks.begin(); it != sub_tasks.end(); ++it)
    {
        sub((*it).second);
    }
    sub_tasks.clear();
    for (auto it = unsub_tasks.begin(); it != unsub_tasks.end(); ++it)
    {
        unsub(*it);
    }
    unsub_tasks.clear();

}

void epollfd::subscribe(int fd, uint32_t ev,
        cont_t cont, cont_t cont_err)
{
    uint32_t &curr_ee = events[fd];
    if (curr_ee & ev)
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
    if ((curr_events & ev) != ev)
    {
        throw std::runtime_error("unsubscribe : doesn't exist");
    }
    unsub_tasks.insert({fd, ev});
}

void epollfd::sub(const sub_task &task)
{
    uint32_t curr_events = events[task.fd];
    uint32_t new_events = curr_events | task.events;
    int op = task.events == new_events ?
        EPOLL_CTL_ADD : EPOLL_CTL_MOD;
    struct epoll_event ee;
    ee.events = new_events;
    ee.data.fd = task.fd;
    if (epoll_ctl(epfd, op, task.fd, &ee))
    {
        throw std::runtime_error("sub : epoll_ctl");
    }
    actions[{task.fd, task.events}] = {task.cont, task.cont_err};
    events[task.fd] = new_events;
}

void epollfd::unsub(const fdev &task)
{
    int fd = task.first;
    uint32_t ev = task.second;
    uint32_t curr_events = events[fd];
    uint32_t new_events = curr_events ^ ev;
    int op = new_events == 0 ? EPOLL_CTL_DEL : EPOLL_CTL_MOD;
    struct epoll_event ee;
    ee.events = new_events;
    ee.data.fd = fd;
    if (epoll_ctl(epfd, op, fd, &&ee))
    {
        throw std::runtime_error("unsub : epoll_ctl");
    }
    actions.erase({fd, ev});
    events[fd] = new_events;
}
