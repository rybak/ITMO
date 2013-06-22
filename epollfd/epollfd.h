#ifndef EPOLLFD_H
#define EPOLLFD_H
#include <ostream>

#include <functional>
#include <set>
#include <map>

typedef std::function<void(void)> cont_t;

struct epollfd
{
    epollfd();
    epollfd(const epollfd &) = delete;
    epollfd & operator=(const epollfd *) = delete;
    epollfd(epollfd &&) = delete;
    epollfd & operator=(epollfd &&) = delete;
    ~epollfd();

    void subscribe(int, uint32_t, cont_t, cont_t);
    void unsubscribe(int, uint32_t);
}

#endif
