// socket_test.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "socket_test.h"
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// The one and only application object

CWinApp theApp;

using namespace std;

#pragma comment (lib, "ws2_32")
static bool init_sock_lib()
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


static const char* strSockError(int errorCode)
{
	for (int i=0; sock_err_map[i].code; i++) {
		if (errorCode == sock_err_map[i].code) {
			return sock_err_map[i].errStr;
		}
	}
	return "unknown socket error:";
}
int _tmain(int argc, TCHAR* argv[], TCHAR* envp[])
{
	
	WSADATA wsaData;
	WSAStartup(MAKEWORD(2,2), &wsaData);

	SOCKET sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	
	if (sock == INVALID_SOCKET) {
		BHJDEBUG(" Error: %s", strSockError(WSAGetLastError()));
		return -1;
	}

	unsigned long sock_opt = 1;
	ioctlsocket(sock, FIONBIO, &sock_opt);

	sockaddr_in client_addr = {0};
	client_addr.sin_family = AF_INET;
	client_addr.sin_port = htons(7);
	if (argc == 1) {
		client_addr.sin_addr.s_addr = inet_addr("192.168.11.150");
	} else {
		client_addr.sin_addr.s_addr = inet_addr(argv[1]);
	}
	int ret = connect(sock, (const sockaddr*)&client_addr, sizeof(client_addr));
	if (ret) {
		int err = WSAGetLastError();
		BHJDEBUG(" Error: %s %d", strSockError(err), err);
	}

	fd_set fd_w;
	FD_ZERO(&fd_w);
	FD_SET(sock, &fd_w);
	
	fd_set fd_e;
	FD_ZERO(&fd_e);
	FD_SET(sock, &fd_e);

	struct timeval to = {2, 0};
	ret = select(0, NULL, &fd_w, &fd_e, &to);
	if (ret == SOCKET_ERROR) {
		int err = WSAGetLastError();
		BHJDEBUG(" Error: select %s %d", strSockError(err), err);
		return -1;
	}  else if (ret == 0) {
		BHJDEBUG(" Error: timeout");
		return -1;
	}else if (FD_ISSET(sock, &fd_w)) {
		BHJDEBUG(" Yes, connect OK");
	} else if (FD_ISSET(sock, &fd_e)) {
		int err = WSAGetLastError();
		BHJDEBUG(" Error: connect %s %d", strSockError(err), err);
		return -1;
	}

	sock_opt = 0;
	ioctlsocket(sock, FIONBIO, &sock_opt);

	char buff[1024] ="hello world\n";
	while (true) {
		ret = send(sock, buff, strlen(buff), 0);
		if (ret == SOCKET_ERROR) {
			BHJDEBUG(" Error: %s", strSockError(WSAGetLastError()));
			return -1;
		} else if (ret != strlen(buff)) {
			BHJDEBUG(" Error: not all data is sent");
			return -1;
		}

		memset(buff, 0, 1024);
		ret = recv(sock, buff, 512, 0);

		if (ret == SOCKET_ERROR) {
			BHJDEBUG(" Error: %s", strSockError(WSAGetLastError()));
			return -1;
		} else {
			buff[ret] = 0;
			fputs(buff, stdout);
		}
	}
				
	
	return 0;
}
