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

#define MAXBUF 65536

int main(int argc, char*argv[])
{
  int sock, status, buflen, sinlen;
  char buffer[MAXBUF];
  struct sockaddr_in sock_in;
  int yes = 1;

  sinlen = sizeof(struct sockaddr_in);
  memset(&sock_in, 0, sinlen);
  buflen = MAXBUF;

  sock = socket (PF_INET, SOCK_DGRAM, IPPROTO_UDP);

  sock_in.sin_addr.s_addr = htonl(INADDR_ANY);
  sock_in.sin_port = htons(0);
  sock_in.sin_family = PF_INET;

  status = bind(sock, (struct sockaddr *)&sock_in, sinlen);
  printf("Bind Status = %d\n", status);
  struct sockaddr addr;
  char ip[INET_ADDRSTRLEN + 1];
  memset(ip, 0, INET_ADDRSTRLEN + 1);
  socklen_t addr_len = sizeof(sockaddr);
  memset(&addr, 0, addr_len);
  status = getsockname(sock, &addr, &addr_len); 
  inet_ntop(addr.sa_family, &addr, ip, addr_len);
  status = setsockopt(sock, SOL_SOCKET, SO_BROADCAST, &yes, sizeof(int) );
  printf("IP := %s\n", ip);

  /* -1 = 255.255.255.255 this is a BROADCAST address,
     a local broadcast address could also be used.
     you can comput the local broadcat using NIC address and its NETMASK 
  */ 

  sock_in.sin_addr.s_addr=htonl(-1); /* send message to 255.255.255.255 */
  sock_in.sin_port = htons(atoi(argv[1])); /* port number */
  sock_in.sin_family = PF_INET;

  for(;;)
  {
      sprintf(buffer, "%s" , argv[2]);
      buflen = strlen(buffer);
      status = sendto(sock, buffer, buflen, 0, (struct sockaddr *)&sock_in, sinlen);
      printf("sendto Status = %d\n", status);
      sleep(5);
  } 
  shutdown(sock, 2);
  close(sock);
}
