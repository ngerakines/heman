/*
Copyright (c) 2010 Nick Gerakines <nick at gerakines dot net>
*/

#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/queue.h>
#include <stdlib.h>
#include <err.h>
#include <string.h>
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h> 
#include <stdlib.h> 
#include <stdio.h>

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <string.h>

#include <getopt.h>
#include <signal.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>

#include <bert/encoder.h>
#include <bert/magic.h>
#include <bert/errno.h>

typedef char byte_t;

void send_command_call(int sd, char *module, char *function, char *arguments);

int main(int argc, char **argv) {
	char *ipaddress = "127.0.0.1";
	int port = 8002;

	int c;
	while (1) {
		static struct option long_options[] = {
			{"ip",      required_argument, 0, 'i'},
			{"port",    required_argument, 0, 'p'},
			{0, 0, 0, 0}
		};
		int option_index = 0;
		c = getopt_long(argc, argv, "i:p:", long_options, &option_index);
		if (c == -1) { break; }
		switch (c) {
			case 0:
				if (long_options[option_index].flag != 0) { break; }
				printf ("option %s", long_options[option_index].name);
				if (optarg) { printf(" with arg %s", optarg); }
				printf("\n");
				break;
			case 'i':
				ipaddress = optarg;
				break;
			case 'p':
				port = atoi(optarg);
				break;
			case '?':
				/* getopt_long already printed an error message. */
				break;
			default:
				abort();
		}
	}

	int action = -1;

	if (argc - optind == 0) {
		printf("Command not provided.\n");
		printf("usage: client [--ip=] [--port=] <command> [... command arguments]\n");
		exit(1);
	}

	if (strcmp(argv[optind], "call") == 0) {
		if (argc - optind != 4) {
			printf("The 'call' command requires 3 command parameters.\n");
			printf("usage: client [--ip=] [--port=] call <module> <function> <arguments>\n");
			exit(1);
		}
		action = 1;
	}

	if (action == 0) {
		printf("Invalid command given, should be either update, next, peek or info.\n");
		printf("usage: client [--ip=] [--port=] <command> [... command arguments]\n");
		exit(1);
	}

	struct hostent *hp;
	struct sockaddr_in pin;
	int sd;

	if ((hp = gethostbyname(ipaddress)) == 0) {
		perror("gethostbyname");
		exit(1);
	}

	memset(&pin, 0, sizeof(pin));
	pin.sin_family = AF_INET;
	pin.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;
	pin.sin_port = htons(port);

	if ((sd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		perror("socket");
		exit(1);
	}

	if (connect(sd,(struct sockaddr *)  &pin, sizeof(pin)) == -1) {
		perror("connect");
		exit(1);
	}

	switch (action) {
		case 1:
			send_command_call(sd, argv[optind + 1], argv[optind + 2], argv[optind + 3]);
			break;
		default:
			break;
	}

	close(sd);

	return 0;
}

#define OUTPUT_SIZE	1024

void send_command_call(int sd, char *module, char *function, char *arguments) {
	unsigned char output[OUTPUT_SIZE];
	bert_encoder_t *encoder;

	printf("creating encoder\n");
	if (!(encoder = bert_encoder_create()))
	{
		printf("malloc failed\n");
	}

	bert_data_t *data;
	int result;
	printf("about to create bert data.\n");
	
	if (!(data = bert_data_create_tuple(4)))
	{
		printf("malloc failed\n");
	}

	if (!(data->tuple->elements[0] = bert_data_create_atom("call")))
	{
		printf("malloc 1 failed\n");
	}
	if (!(data->tuple->elements[1] = bert_data_create_string(module)))
	{
		printf("malloc 2 failed");
	}
	if (!(data->tuple->elements[2] = bert_data_create_string(function)))
	{
		printf("malloc 3 failed");
	}
	printf("bytes written: %lu\n", bert_encoder_total(encoder));
	if (!(data->tuple->elements[3] = bert_data_create_string(arguments)))
	{
		printf("malloc 4 failed");
	}
	printf("bytes written: %lu\n", bert_encoder_total(encoder));

	printf("preparing encode buffer\n");
	bert_encoder_buffer(encoder, output, OUTPUT_SIZE);
	printf("bytes written: %lu\n", bert_encoder_total(encoder));

	if ((result = bert_encoder_push(encoder, data)) != BERT_SUCCESS)
	{
		printf("HMMMM: %s\n", bert_strerror(result));
	}
	printf("bytes written: %lu\n", bert_encoder_total(encoder));

	printf("output string is '%s'\n", output);

	if (send(sd, output, sizeof(output), 0) == -1) {
		perror("send");
		exit(1);
	}
	
	bert_data_destroy(data);
	bert_encoder_destroy(encoder);
	
	printf("sent!\n");

	char buf[1024];
	int numbytes;
	if((numbytes = recv(sd, buf, 1024-1, 0)) == -1) {
		perror("recv()");
		exit(1);
	}

	printf("%s\n", buf);

}
