#include <unistd.h>
#include "autofd.h"

autofd::autofd(int fd)
    : fd(fd)
{ }

autofd::~autofd()
{
    if (fd >= 0)
    {
        close(fd);
    }
}

autofd(autofd &&b)
    : fd(b.fd)
{
    b.fd = wrong_fd;
}

autofd & autofd::operator=(autofd &&b)
{
    fd = b.fd;
    b.fd = wrong_fd;
}

