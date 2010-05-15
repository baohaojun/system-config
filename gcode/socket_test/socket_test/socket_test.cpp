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


int _tmain(int argc, TCHAR* argv[], TCHAR* envp[])
{
	
	WSADATA wsaData;
	WSAStartup(MAKEWORD(2,2), &wsaData);



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
