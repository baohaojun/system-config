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


/* doconnect :
   do all the socket stuff, and return an fd for one of
   an open outbound TCP connection
   a UDP stub-socket thingie
   with appropriate socket options set up if we wanted source-routing, or
   an unconnected TCP or UDP socket to listen on.
   Examines various global o_blah flags to figure out what-all to do. */
int doconnect (IA * rad, USHORT rp, IA * lad, USHORT lp)
{
	register int nnetfd;
	register int rr;
	int x, y;
	errno = 0;

	/* grab a socket; set opts */
newskt:
	nnetfd = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (nnetfd < 0)
		bail ("Can't get socket");
	if (nnetfd == 0)		/* if stdin was closed this might *be* 0, */
		goto newskt;		/* so grab another.  See text for why... */
	x = 1;
	rr = setsockopt (nnetfd, SOL_SOCKET, SO_REUSEADDR, &x, sizeof (x));
	if (rr == -1)
		holler ("nnetfd reuseaddr failed");		/* ??? */

	return (nnetfd);
	close (nnetfd);			/* clean up junked socket FD!! */
	return (-1);
} /* doconnect */

int main(int argc, char** argv)
{

}
