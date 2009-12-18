#ifdef WIN32
#include <windef.h>
#include <winsock.h>
#else
#include <errno.h>
#include <netdb.h>
#include <sys/socket.h>
#endif /* WIN32 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sockwrapc.h"

typedef struct
{
  long int socket;
  struct sockaddr_in addr;
  int addrtype;
} SocketInfo;

void socket_init_win32()
{
#ifdef WIN32
  WSADATA data;

  if (WSAStartup(MAKEWORD(1, 1), &data) != 0)
  {
    WSACleanup();
  }
#endif
}

int socket_init(char * hostname, int port)
{
  struct hostent * host;
  struct sockaddr_in addr;

  SocketInfo * info = (SocketInfo *) malloc(sizeof(SocketInfo));

  socket_init_win32();

  if ((host = gethostbyname(hostname)) == NULL)
  {
    return SOCKET_ERROR;
  }

  memset(&addr, 0, sizeof(addr));
  memcpy((char *) &addr.sin_addr, host->h_addr, host->h_length);
  
  addr.sin_family = host->h_addrtype;
  addr.sin_port = htons((u_short) port);
  info->addr = addr;

  info->socket = INVALID_SOCKET;
  info->addrtype = host->h_addrtype;
  return (int) info;
}

void socket_done(int sock)
{
  SocketInfo * info = (SocketInfo *) sock;

  if (info->socket != INVALID_SOCKET)
  {
#ifdef WIN32
    closesocket(info->socket);
    WSACleanup();
#else
    close(info->socket);
#endif
  }

  free(info);
}

void socket_connect(int sock)
{
  SocketInfo * info = (SocketInfo *) sock;
  info->socket = socket(info->addrtype, SOCK_STREAM, 0);
  connect(info->socket, (struct sockaddr *) &info->addr, sizeof(info->addr));
}

int socket_send(int sock, char * data)
{
  int ret;
  SocketInfo * info = (SocketInfo *) sock;

  ret = send(info->socket, data, strlen(data), 0);
  return ret;
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
    else if (count == SOCKET_ERROR)
    {
      return SOCKET_ERROR;
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
