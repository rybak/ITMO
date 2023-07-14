#include <sys/stat.h>
#include <exception>

#include "files.h"

int filesize(const char *filename)
{
    stat st;
    if (stat(filename, &st) == 0)
    {
        return st.st_size;
    } else {
        perror("Filesize : ");
        throw std::exception("files.h : filesize");
    }
}
