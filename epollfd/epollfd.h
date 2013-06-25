#ifndef EPOLLFD_H
#define EPOLLFD_H

#include <functional>
#include <set>
#include <map>

typedef std::function<void(void)> cont_t;

struct sub_task
{
    int fd;
    uint32_t events;
    cont_t cont;
    cont_t cont_err;
};

struct epollfd
{
    static const int MAX_EVENTS = 10;
    epollfd(int max_events = MAX_EVENTS);
    epollfd(const epollfd &) = delete;
    epollfd & operator=(const epollfd *) = delete;
    epollfd(epollfd &&) = delete;
    epollfd & operator=(epollfd &&) = delete;
    ~epollfd();

    void subscribe(int, uint32_t, cont_t, cont_t);
    void unsubscribe(int, uint32_t);
    void cycle();
private:
    int epfd;
    int max_events;
    typedef std::pair<int, uint32_t> fdev;
    std::map<fdev, sub_task> sub_tasks;
    std::set<fdev> unsub_tasks;
    std::map<int, uint32_t> events;
    std::map<int, std::map<uint32_t, std::pair<cont_t, cont_t> > >
        actions;
    void sub(const sub_task &);
    void unsub(const fdev &);
};

#endif
