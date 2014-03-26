#include <windows.h>
#include <winerror.h>
#include <memory.h>
#include <immdev.h>
#include <imedefs.h>
#include "imewnd.h"
#include <map>
#include <string>
#include <vector>
#include <winsock.h>
#include "ime-socket.h"

using namespace std;
#pragma comment (lib, "ws2_32")

static SOCKET g_ime_sock;
static struct {
	int code;
	const char *errStr;
} sock_err_map[] = {
	{WSAEINTR, "WSAEINTR"},
	{WSAEBADF, "WSAEBADF"},
	{WSAEACCES, "WSAEACCES"},
	{WSAEFAULT, "WSAEFAULT"},
	{WSAEINVAL, "WSAEINVAL"},
	{WSAEMFILE, "WSAEMFILE"},
	{WSAEWOULDBLOCK, "WSAEWOULDBLOCK"},
	{WSAEINPROGRESS, "WSAEINPROGRESS"},
	{WSAEALREADY, "WSAEALREADY"},
	{WSAENOTSOCK, "WSAENOTSOCK"},
	{WSAEDESTADDRREQ, "WSAEDESTADDRREQ"},
	{WSAEMSGSIZE, "WSAEMSGSIZE"},
	{WSAEPROTOTYPE, "WSAEPROTOTYPE"},
	{WSAENOPROTOOPT, "WSAENOPROTOOPT"},
	{WSAEPROTONOSUPPORT, "WSAEPROTONOSUPPORT"},
	{WSAESOCKTNOSUPPORT, "WSAESOCKTNOSUPPORT"},
	{WSAEOPNOTSUPP, "WSAEOPNOTSUPP"},
	{WSAEPFNOSUPPORT, "WSAEPFNOSUPPORT"},
	{WSAEAFNOSUPPORT, "WSAEAFNOSUPPORT"},
	{WSAEADDRINUSE, "WSAEADDRINUSE"},
	{WSAEADDRNOTAVAIL, "WSAEADDRNOTAVAIL"},
	{WSAENETDOWN, "WSAENETDOWN"},
	{WSAENETUNREACH, "WSAENETUNREACH"},
	{WSAENETRESET, "WSAENETRESET"},
	{WSAECONNABORTED, "WSAECONNABORTED"},
	{WSAECONNRESET, "WSAECONNRESET"},
	{WSAENOBUFS, "WSAENOBUFS"},
	{WSAEISCONN, "WSAEISCONN"},
	{WSAENOTCONN, "WSAENOTCONN"},
	{WSAESHUTDOWN, "WSAESHUTDOWN"},
	{WSAETOOMANYREFS, "WSAETOOMANYREFS"},
	{WSAETIMEDOUT, "WSAETIMEDOUT"},
	{WSAECONNREFUSED, "WSAECONNREFUSED"},
	{WSAELOOP, "WSAELOOP"},
	{WSAENAMETOOLONG, "WSAENAMETOOLONG"},
	{WSAEHOSTDOWN, "WSAEHOSTDOWN"},
	{WSAEHOSTUNREACH, "WSAEHOSTUNREACH"},
	{WSAENOTEMPTY, "WSAENOTEMPTY"},
	{WSAEPROCLIM, "WSAEPROCLIM"},
	{WSAEUSERS, "WSAEUSERS"},
	{WSAEDQUOT, "WSAEDQUOT"},
	{WSAESTALE, "WSAESTALE"},
	{WSAEREMOTE, "WSAEREMOTE"},
	{WSAEDISCON, "WSAEDISCON"},
	{WSASYSNOTREADY, "WSASYSNOTREADY"},
	{WSAVERNOTSUPPORTED, "WSAVERNOTSUPPORTED"},
	{WSANOTINITIALISED, "WSANOTINITIALISED"},
	{0, NULL},
};


string sock_error()
{
	int errorCode = WSAGetLastError();
	for (int i=0; sock_err_map[i].code; i++) {
		if (errorCode == sock_err_map[i].code) {
			return sock_err_map[i].errStr;
		}
	}
	char buff[1024];
	_snprintf(buff, sizeof(buff), "unknown socket error: %d", errorCode);
	return buff;
}

#define bhj_sock_error(fmt, ...) do {					\
		string err_str = sock_error();					\
		BHJDEBUG("Error: %s " fmt, err_str.c_str(), ##__VA_ARGS__);	\
	} while (0)


static void start_ime_server()
{
    STARTUPINFO           startup;
    PROCESS_INFORMATION   pinfo;
    int                   ret;

    ZeroMemory( &startup, sizeof(startup) );
    startup.cb = sizeof(startup);

    startup.hStdInput  = GetStdHandle(STD_INPUT_HANDLE);		
	startup.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
	startup.hStdError = GetStdHandle(STD_ERROR_HANDLE);

	startup.wShowWindow = SW_HIDE;

    startup.dwFlags    = STARTF_USESHOWWINDOW;

	if (startup.hStdInput && startup.hStdOutput && startup.hStdError) {
		startup.dwFlags |= STARTF_USESTDHANDLES;
	}

    ZeroMemory( &pinfo, sizeof(pinfo) );

	char c_str[1024];
	FILE* fp = fopen("c:/ime-server.rc", "r");
	if (! fp) {
		return;
	}
	fgets(c_str, sizeof c_str, fp);
	fclose(fp);

	wstring w_str = to_wstring(c_str);
	wchar_t buff[1024] = {0};
	memcpy(buff, w_str.c_str(), sizeof (wchar_t) * w_str.size());

    ret = CreateProcess(
		NULL,
		buff, //error for L"q:\\dood.exe", because it must not be const!!!
		NULL, /* process handle is not inheritable */
		NULL, /* thread handle is not inheritable */
		TRUE, /* yes, inherit some handles */
		0, /* the new process doesn't have a console */
		NULL, /* use parent's environment block */
		NULL, /* use parent's starting directory */
		&startup, /* startup info, i.e. std handles */
		&pinfo );

    if (!ret) {
        return;
    }

    CloseHandle( pinfo.hProcess );
    CloseHandle( pinfo.hThread );

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
		if (ret == SOCKET_ERROR) {
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
		if (ret == SOCKET_ERROR) {
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
		closesocket(g_ime_sock);
	}
	g_ime_sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	
	if (g_ime_sock == INVALID_SOCKET) {
		bhj_sock_error("");
		exit(-1);
	}

	unsigned long sock_opt = 1;
	ioctlsocket(g_ime_sock, FIONBIO, &sock_opt);

	sockaddr_in client_addr = {0};
	client_addr.sin_family = AF_INET;
	client_addr.sin_port = htons(31415);
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
	if (ret == SOCKET_ERROR) {
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
	ioctlsocket(g_ime_sock, FIONBIO, &sock_opt); 
	return;
}

bool init_ime_socket()
{
	WORD wVersionRequested;
	WSADATA wsaData;
	int err;
 
	wVersionRequested = MAKEWORD( 2, 2 );
 
	err = WSAStartup( wVersionRequested, &wsaData );
	if ( err != 0 ) {
		return false;
	}
 
	if ( LOBYTE( wsaData.wVersion ) != 2 ||
		 HIBYTE( wsaData.wVersion ) != 2 ) {
		WSACleanup( );
		return false; 
	}
	return true;
}
