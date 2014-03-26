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

static int g_ime_sock = -1;

string sock_error()
{
	int err = errno;
	errno = 0; //clear the error!
	return strerror(err);
}

#define bhj_sock_error(fmt, ...) do {								\
		string err_str = sock_error();								\
		BHJDEBUG("Error: %s " fmt, err_str.c_str(), ##__VA_ARGS__);	\
	} while (0)

#define ignore 1+
static void start_ime_server()
{
	ignore system("python3 ~/gcode/scim-cs/ime-py/ime-server.py >/dev/null 2>&1 &");
}

void ime_write_line(const string& line)
{
start:
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
		if (ret <= 0) {
			bhj_sock_error("ime_recv_line: ");
			connect_ime_server();
			return "end:"; //this is a hack
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
	if (g_ime_sock >= 0) {
		close(g_ime_sock);
	}
	g_ime_sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	
	if (g_ime_sock < 0) {
		bhj_sock_error("create socket");
		exit(-1);
	}

	struct sockaddr_in client_addr = {0};
	client_addr.sin_family = AF_INET;
	client_addr.sin_port = htons(31415);
	client_addr.sin_addr.s_addr = inet_addr("127.0.0.1");

	int ret = connect(g_ime_sock, (const sockaddr*)&client_addr, sizeof(client_addr));

	if (ret < 0) {
		start_ime_server();
		bhj_sock_error("connect error");
		system("sleep 1");
		goto start;
	}
	return;
}

#ifdef IME_SOCK_TEST
int main()
{
	connect_ime_server();
	while(true) {
		char buff[1024];
		printf(">");
		fflush(stdout);
		fgets(buff, 1024, stdin);
		ime_write_line(buff);
		string answer = ime_recv_line();
		if (answer.empty() || answer == "end:") {
			continue;
		}
	}
	return 0;
}
#endif
