#ifndef WIN32
#define INVALID_SOCKET -1
#define SOCKET_ERROR -1
#endif

int socket_init(char * hostname, char * port);
void socket_done(int sock);

void socket_connect(int sock);
int socket_send(int sock, char * data);
int socket_receive(int sock, char *buf, int len);

int socket_get_error(int sock);