/* conditional includes -- a very messy section which you may have to dink
   for your own architecture [and please send diffs...]: */
/* #undef _POSIX_SOURCE		/* might need this for something? */
#define HAVE_BIND		/* ASSUMPTION -- seems to work everywhere! */
#define HAVE_HELP		/* undefine if you dont want the help text */
/* #define ANAL			/* if you want case-sensitive DNS matching */

#include <stdlib.h>
#include <malloc.h>

/* have to do this *before* including types.h. xxx: Linux still has it wrong */
#ifdef FD_SETSIZE		/* should be in types.h, butcha never know. */
#undef FD_SETSIZE		/* if we ever need more than 16 active */
#endif				/* fd's, something is horribly wrong! */
#define FD_SETSIZE 16		/* <-- this'll give us a long anyways, wtf */
#include <sys/types.h>		/* *now* do it.  Sigh, this is broken */

#define SRAND srandom		/* that this doesn't need *strong* random */
#define RAND random		/* numbers just to mix up port numbers!! */

/* #define POSIX_SETJMP		/* If you want timeouts to work under the */
/* posixly correct, yet non-standard glibc-2.x*/
/* then define this- you may also need it for */
/* IRIX, and maybe some others */
#define POSIX_SETJMP

/* includes: */
#include <sys/time.h>		/* timeval, time_t */
#include <setjmp.h>		/* jmp_buf et al */
#include <sys/socket.h>		/* basics, SO_ and AF_ defs, sockaddr, ... */
#include <netinet/in.h>		/* sockaddr_in, htons, in_addr */
#include <netinet/in_systm.h>	/* misc crud that netinet/ip.h references */
#include <netinet/ip.h>		/* IPOPT_LSRR, header stuff */
#include <netdb.h>		/* hostent, gethostby*, getservby* */
#include <arpa/inet.h>		/* inet_ntoa */
#include <stdio.h>
#include <string.h>		/* strcpy, strchr, yadda yadda */
#include <errno.h>
#include <signal.h>
#include <fcntl.h>		/* O_WRONLY et al */
#include <resolv.h>
#include <unistd.h>

/* handy stuff: */
#define SA struct sockaddr	/* socket overgeneralization braindeath */
#define SAI struct sockaddr_in	/* ... whoever came up with this model */
#define IA struct in_addr	/* ... should be taken out and shot, */
/* ... not that TLI is any better.  sigh.. */
#define SLEAZE_PORT 31337	/* for UDP-scan RTT trick, change if ya want */
#define USHORT unsigned short	/* use these for options an' stuff */
#define BIGSIZ 8192		/* big buffers */

#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif
#ifdef MAXHOSTNAMELEN
#undef MAXHOSTNAMELEN		/* might be too small on aix, so fix it */
#endif
#define MAXHOSTNAMELEN 256


void die(const char* msg)
{
	fprintf(stderr, "%s\n", msg);
	exit(-1);
}

int get_network_fd()
{
	int fd = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (fd < 0)
		die("Can't get socket");
	int x = 1;
	int rr = setsockopt (fd, SOL_SOCKET, SO_REUSEADDR, &x, sizeof (x));
	if (rr == -1)
		warn("nnetfd reuseaddr failed");

	return fd;
}

int start_server(int srv_fd)
{
	struct sockaddr_in sai;
	sai.sin_family = AF_INET;
	sai.sin_port = htons(54321);
	sai.sin_addr.s_addr = inet_addr("127.0.0.1");
	bind(srv_fd, (struct sockaddr*)&sai, sizeof(struct sockaddr));
	listen(srv_fd, 5);
}

int handle_clients(int srv_fd)
{
	while (1) {
		int conn_fd = accept(srv_fd, NULL, NULL);
		pid_t pid = fork();
		if (pid == 0) { //child
			dup2(conn_fd, 0);
			dup2(conn_fd, 1);
			dup2(conn_fd, 2);
			execlp("tcmd-client-handler", "tcmd-client-handler", (char*) NULL);
		}
		close(conn_fd);
	}
}
int main(int argc, char** argv)
{
	int srv_fd;
	srv_fd = get_network_fd();
	start_server(srv_fd);
	handle_clients(srv_fd);
}
