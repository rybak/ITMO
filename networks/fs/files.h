#ifndef FILES_H
#define FILES_H

#include <string>

struct nfs_file
{
    std::string get_system_filename() const;
private:
    std::string name;
    int size;
};

int filesize(const char *);

#endif
