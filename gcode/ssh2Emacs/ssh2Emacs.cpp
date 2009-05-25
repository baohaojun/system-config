// ssh2Emacs.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <Winsock2.h>
#include "bhjdebug.h"
#include <stdlib.h>
#include <string.h>

DWORD WINAPI SocketThread(LPVOID lpParameter)
{
    SOCKET read_sock = (SOCKET)lpParameter;
    char buff[1024] = {0};
    recv(read_sock, buff, 1024, 0);
    BHJDEBUG("received %s", buff);
    if (!strncmp(buff, "redit ", strlen("redit ")) ||
        !strncmp(buff, "ropen ", strlen("ropen "))) {
        system(buff);
    }
    printf("%s\n", buff);
    fflush(stdout);
    closesocket(read_sock);
    wchar_t wbuff[1024]={0};
    return 0;
}

int _tmain(int argc, _TCHAR* argv[])
{
    WSADATA wsa;
    WSAStartup(MAKEWORD(2,0), &wsa);
    SOCKET sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sock == INVALID_SOCKET)
    {
        BHJDEBUG("%s", "hello");
        WSACleanup ();
        return 0;
    }

    struct sockaddr_in sa;
    sa.sin_family           = AF_INET;
    sa.sin_port             = htons (3456); 
    sa.sin_addr.S_un.S_addr = inet_addr ("127.0.0.1");

    bind(sock, (struct sockaddr*)&sa, sizeof(sa));

    listen(sock, SOMAXCONN);

    while(true) {

        SOCKET read_sock = accept(sock, (struct sockaddr*)&sa, NULL);
        HANDLE g_hThreadPv = CreateThread(NULL, 0, SocketThread, (LPVOID)read_sock, 0, NULL);
        CloseHandle(g_hThreadPv);
    }
	return 0;
}

