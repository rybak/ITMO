#include <sys/ioctl.h>
#include <net/if.h> 
#include <netinet/in.h> 
#include <string.h>
#include <cstdlib>

#include "common.h"

#include "ma.h"

#include <cstdio>

void mov(_mac_addr_t &to, const _mac_addr_t &from)
{
    for (int i = 0; i < 6; ++i)
    {
        to[i] = from[i];
    }
}

void print_mac(const mac_addr_t &ma)
{
    printf("%02x:%02x:%02x:%02x:%02x:%02x", ma.ma[0], ma.ma[1], ma.ma[2], ma.ma[3], ma.ma[4], ma.ma[5]);
}

void get_mac(mac_addr_t &dest_ma)
{
    static bool MAC_CALC = false;
    static mac_addr_t ma;
    if (!MAC_CALC)
    {
        struct ifreq ifr;
        struct ifconf ifc;
        char buf[1024];
        int success = 0;
        int sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_IP);
        if (sock == -1)
        {
            die("getmac");
        };

        ifc.ifc_len = sizeof(buf);
        ifc.ifc_buf = buf;
        if (ioctl(sock, SIOCGIFCONF, &ifc) == -1)
        {
            die("announcer : ioctl");
        }

        struct ifreq* it = ifc.ifc_req;
        const struct ifreq* const end = it + (ifc.ifc_len / sizeof(struct ifreq));

        for (; it != end; ++it)
        {
            strcpy(ifr.ifr_name, it->ifr_name);
            if (ioctl(sock, SIOCGIFFLAGS, &ifr) == 0)
            {
                if (! (ifr.ifr_flags & IFF_LOOPBACK))
                { // don't count loopback
                    if (ioctl(sock, SIOCGIFHWADDR, &ifr) == 0)
                    {
                        success = 1;
                        break;
                    }
                }
            }
            else { /* handle error */ }
        }

        if (success)
        {
            MAC_CALC = true;
            memcpy(ma.ma, ifr.ifr_hwaddr.sa_data, 6);
        } else
        {
            die("get_mac");
        }
    }
    memcpy(&dest_ma, &ma, sizeof(ma));
}


