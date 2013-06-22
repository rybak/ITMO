#include <unistd.h>

#include <utility>

#include "autofd.h"

autofd::autofd(int fd)
{
    if (fd >= 0)
    {
        this->fd = fd;
    } else
    {

    }
}

autofd::~autofd()
{
    if (fd >= 0)
    {
        close(fd);
    }
}

autofd::autofd(autofd &&b)
    : fd(b.fd)
{
    b.fd = wrong_fd;
}

autofd & autofd::operator=(autofd &&b)
{
    std::swap(fd, b.fd);
}

