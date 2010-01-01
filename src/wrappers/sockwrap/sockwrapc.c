#ifdef WIN32
#include <windef.h>
#include <ws2tcpip.h>
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
  int error;
  struct addrinfo * info;
} SocketInfo;

void socket_init_win32()
{
#ifdef WIN32
  WSADATA data;
  WSAStartup(MAKEWORD(2, 2), &data);
#endif
}

int socket_get_last_error()
{
#ifdef WIN32
    return WSAGetLastError();
#else
      return errno;
#endif
}


int socket_init(char * hostname, char * port)
{
  int err;
  struct addrinfo * ai;
  SocketInfo * info = (SocketInfo *) malloc(sizeof(SocketInfo));

  socket_init_win32();
  
  err = getaddrinfo(hostname, port, NULL, &ai);
  if (err != 0)
  {
    printf("%s\n", gai_strerror(err));
    return SOCKET_ERROR;
  }

  info->info = ai;
  info->socket = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);

  info->error = socket_get_last_error();
  return (int) info;
}

void socket_done(int sock)
{
  SocketInfo * info = (SocketInfo *) sock;

#ifdef WIN32
  closesocket(info->socket);
  WSACleanup();
#else
  close(info->socket);
#endif

  free(info);
}

void socket_connect(int sock)
{
  SocketInfo * info = (SocketInfo *) sock;

  if (info->socket != INVALID_SOCKET)
  {
    connect(info->socket, info->info->ai_addr, info->info->ai_addrlen);
    info->error = socket_get_last_error();
  }

  freeaddrinfo(info->info);
}

int socket_send(int sock, char * data)
{
  int ret;
  SocketInfo * info = (SocketInfo *) sock;

  ret = send(info->socket, data, strlen(data), 0);
  info->error = socket_get_last_error();

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

  info->error = socket_get_last_error();
  return total;
}

int socket_get_error(int sock)
{
  SocketInfo * info = (SocketInfo *) sock;
  return info->error;
}
