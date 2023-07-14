#ifndef AUTO_FD_H
#define AUTO_FD_H

struct autofd
{
    autofd(int);
    ~autofd();

    autofd(const autofd &) = delete;
    autofd& operator=(const autofd &) = delete;

    autofd(autofd &&);
    autofd& operator=(autofd &&);
private:
    static const int wrong_fd = -1;
    int fd;
};

#endif
