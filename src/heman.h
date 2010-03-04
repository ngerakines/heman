
struct client {
	struct event ev_read;
};

int main(int argc, char **argv);

void on_read(int fd, short ev, void *arg);
void on_accept(int fd, short ev, void *arg);
int setnonblock(int fd);
void reply(int fd, char *buffer);
