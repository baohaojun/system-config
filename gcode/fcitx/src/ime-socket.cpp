#include <map>
#include <string>
#include <vector>
#include "ime-socket.h"
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>


using namespace std;

static int g_ime_sock;

string sock_error()
{
	return strerror(errno);
}

#define bhj_sock_error(fmt, ...) do {								\
		string err_str = sock_error();								\
		BHJDEBUG("Error: %s " fmt, err_str.c_str(), ##__VA_ARGS__);	\
	} while (0)

#define ignore 1+
static void start_ime_server()
{
	ignore system("python3.1 ~/gcode/scim-cs/ime-py/ime-server.py&");
}

void ime_write_line(const string& line)
{
start:
	BHJDEBUG(" begine write line");
	string str = line;
	str.push_back('\n');

	size_t total = str.size();
	size_t n = 0;

	while (n < total) {
		int ret = send(g_ime_sock, str.substr(n).c_str(), total - n, 0);
		if (ret < 0) {
			bhj_sock_error("ime_write_line: ");
			connect_ime_server();
			goto start;
		}
		n += ret;
	}
}

string ime_recv_line()
{
	string str;
	int cap = 1024;
	str.reserve(cap);
	for (int i=0; ;i++ ) {
		if (i == cap) {
			cap = (int)(cap * 1.5);
			str.reserve(cap);
		}
		char c;
		int ret = recv(g_ime_sock, &c, 1, 0);
		if (ret < 0) {
			bhj_sock_error("ime_recv_line: ");
			connect_ime_server();
			return "";
		}
		if (c == '\n') {
			return str;
		} else {
			str.push_back(c);
		}
	}
}

void connect_ime_server()
{
start:
	if (g_ime_sock) {
		close(g_ime_sock);
	}
	g_ime_sock = socket(AF_INET, SOCK_STREAM, 0);
	
	if (g_ime_sock < 0) {
		bhj_sock_error("");
		exit(-1);
	}

	unsigned long sock_opt = 1;
	ioctl(g_ime_sock, FIONBIO, &sock_opt);

	struct sockaddr_in client_addr = {0};
	client_addr.sin_family = AF_INET;
	client_addr.sin_port = htons(12345);
	client_addr.sin_addr.s_addr = inet_addr("127.0.0.1");

	int ret = connect(g_ime_sock, (const sockaddr*)&client_addr, sizeof(client_addr));
	if (ret) {
		bhj_sock_error("");
	}

	fd_set fd_w;
	FD_ZERO(&fd_w);
	FD_SET(g_ime_sock, &fd_w);
	
	fd_set fd_e;
	FD_ZERO(&fd_e);
	FD_SET(g_ime_sock, &fd_e);

	struct timeval to = {1, 0};
	ret = select(0, NULL, &fd_w, &fd_e, &to);
	if (ret < 0) {
		bhj_sock_error(""); 
		start_ime_server();
		goto start;
	}  else if (ret == 0) {
		start_ime_server();
		goto start;
	}else if (FD_ISSET(g_ime_sock, &fd_w)) {
	} else if (FD_ISSET(g_ime_sock, &fd_e)) {
		bhj_sock_error(""); 
		start_ime_server();
		goto start;
	}
	sock_opt = 0;
	ioctl(g_ime_sock, FIONBIO, &sock_opt); 
	return;
}

