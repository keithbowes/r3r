int socket_init(char * hostname, int port);
void socket_done(int sock);

void socket_connect();
int socket_send(int sock, char * data);
int socket_receive(int sock, char *buf, int len);

int socket_get_error();
