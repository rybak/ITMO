#ifndef MA_H
#define MA_H

typedef unsigned char _mac_addr_t[6];

typedef union
{
    _mac_addr_t ma;
    long long id;

} mac_addr_t;

void get_mac(mac_addr_t &);

void print_mac(const mac_addr_t &);

void mov(_mac_addr_t &, const _mac_addr_t &);

#endif

