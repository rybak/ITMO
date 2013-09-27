#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <poll.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <iostream>
#include <ifaddrs.h>

#define MAXBUF 65536

#include "common.h"


using namespace std;
void print_ip()
{
    struct ifaddrs *ifAddrStruct = NULL;
    struct ifaddrs *ifa = NULL;
    void *tmpAddrPtr = NULL;

    cout << "1" << endl;
    if (getifaddrs(&ifAddrStruct) < 0)
    {
        die("getifaddrs");
    }
    cout << "2" << endl;
    for (ifa = ifAddrStruct; ifa != NULL; ifa = ifa->ifa_next)
    {
        if (ifa->ifa_addr != NULL)
        {
            if (ifa->ifa_addr->sa_family == AF_INET)
            {
                cout << "3" << endl;
                tmpAddrPtr = &((struct sockaddr_in *)ifa->ifa_addr)
                    ->sin_addr;
                char addressBuffer[INET_ADDRSTRLEN + 1];
                memset(addressBuffer, 0, INET_ADDRSTRLEN + 1);
                inet_ntop(AF_INET, tmpAddrPtr,
                        addressBuffer, INET_ADDRSTRLEN);
                printf("%s IP Address %s\n", ifa->ifa_name, addressBuffer); 
            } else if (ifa->ifa_addr->sa_family==AF_INET6) {
                cout << "4" << endl;
                tmpAddrPtr=&((struct sockaddr_in6 *)ifa->ifa_addr)
                    ->sin6_addr;
                char addressBuffer[INET6_ADDRSTRLEN + 1];
                memset(addressBuffer, 0, INET6_ADDRSTRLEN + 1);
                inet_ntop(AF_INET6, tmpAddrPtr,
                        addressBuffer, INET6_ADDRSTRLEN);
                printf("%s IP Address %s\n", ifa->ifa_name, addressBuffer); 
            } 
        }
    }
    if (ifAddrStruct!=NULL)
    {
        freeifaddrs(ifAddrStruct);
    }
}

int main(int argc, char*argv[])
{
    int sock;
    sock = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (sock < 0)
    {
        die("socket");
    }

    struct sockaddr_in dest_sock;
    socklen_t si_len = sizeof(dest_sock);
    memset(&dest_sock, 0, si_len);
    dest_sock.sin_addr.s_addr = htonl(INADDR_ANY);
    dest_sock.sin_port = htons(0);
    dest_sock.sin_family = AF_INET;

    int status = bind(sock, (struct sockaddr *) &dest_sock, si_len);
    if (status < 0)
    {
        die("bind");
    }
    
    printf("before\n");
    print_ip();
    printf("\nafter\n");
    int yes = 1;
    status = setsockopt(sock, SOL_SOCKET, SO_BROADCAST, &yes, sizeof(int) );

    /* -1 = 255.255.255.255 this is a BROADCAST address,
       a local broadcast address could also be used.
       you can comput the local broadcat using NIC address and its NETMASK 
       */ 

    dest_sock.sin_addr.s_addr=htonl(-1); /* send message to 255.255.255.255 */
    dest_sock.sin_port = htons(atoi(argv[1])); /* port number */

    char buffer[MAXBUF];
    for(;;)
    {
        sprintf(buffer, "%s" , argv[2]);
        size_t buflen = strlen(buffer);
        status = sendto(sock, buffer, buflen, 0,
                (struct sockaddr *) &dest_sock, si_len);
        if (status < 0)
        {
            dontdie("sendto");
        }
        sleep(5);
    } 
    close(sock);
}

