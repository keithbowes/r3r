#ifdef WIN32
#include <winsock.h>
#else
#include <errno.h>
#include <netdb.h>
#include <sys/socket.h>
#endif /* WIN32 */

#include <stdlib.h>
#include <string.h>

#include "sockwrapc.h"

typedef struct
{
  int socket;
  struct sockaddr_in addr;
} SocketInfo;

int socket_init(char * hostname, int port)
{
  struct hostent * host;
  struct sockaddr_in addr;
  SocketInfo * info = malloc(sizeof(SocketInfo));

  if ((host = gethostbyname(hostname)) == NULL)
  {
    return -1;
  }

  memset(&addr, 0, sizeof(addr));
  memcpy((char *) &addr.sin_addr, host->h_addr, host->h_length);
  
  addr.sin_family = host->h_addrtype;
  addr.sin_port = htons((u_short) port);
  info->addr = addr;

  if ((info->socket = socket(host->h_addrtype, SOCK_STREAM, 0)) < 0)
  {
  }

  return (int) info;
}

void socket_done(int sock)
{
  SocketInfo * info = (SocketInfo *) sock;
#ifdef WIN32
  closesocket(info->socket);
#else
  close(info->socket);
#endif

  free(info);
}

void socket_connect(int sock)
{
  SocketInfo * info = (SocketInfo *) sock;
  connect(info->socket, (struct sockaddr *) &info->addr, sizeof(info->addr));
}

int socket_send(int sock, char * data)
{
  int ret;
  SocketInfo * info = (SocketInfo *) sock;

  if (ret = send(info->socket, data, strlen(data), 0) >= 0)
  {
    return ret;
  }
  else
  {
    return -1;
  }
}

int socket_receive(int sock, char * buf, int len)
{
  int count;
  int total;

  SocketInfo * info = (SocketInfo *) sock;
  
  count = 0;
  total = 0;
  while (total < len)
  {
    if ((count = recv(info->socket, buf, len - total, 0)) > 0)
    {
      total += count;
      buf += count;
    }
    else if (count < 0)
    {
      return -1;
    }
    else
    {
      break;
    }
  }

  return total;
}

int socket_get_error()
{
#ifdef WIN32
  return WSAGetLastError();
#else
  return errno;
#endif
}
