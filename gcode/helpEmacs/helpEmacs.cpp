// helpEmacs.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "helpEmacs.h"

#include <WinSock2.h>
#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// ChelpEmacsApp

BEGIN_MESSAGE_MAP(ChelpEmacsApp, CWinApp)
ON_COMMAND(ID_HELP, &CWinApp::OnHelp)
END_MESSAGE_MAP()


// ChelpEmacsApp construction

ChelpEmacsApp::ChelpEmacsApp()
{
    // TODO: add construction code here,
    // Place all significant initialization in InitInstance
}


// The one and only ChelpEmacsApp object

ChelpEmacsApp theApp;


// ChelpEmacsApp initialization

BOOL ChelpEmacsApp::InitInstance()
{
    // InitCommonControlsEx() is required on Windows XP if an     // manifest specifies use of ComCtl32.dll version 6 or later to enable
    // visual styles.  Otherwise, any window creation will fail.
    INITCOMMONCONTROLSEX InitCtrls;
    InitCtrls.dwSize = sizeof(InitCtrls);
    // Set this to include all the common control classes you want to use
    // in your application.
    InitCtrls.dwICC = ICC_WIN95_CLASSES;
    InitCommonControlsEx(&InitCtrls);

    CWinApp::InitInstance();

    AfxEnableControlContainer();

    // Standard initialization
    // If you are not using these features and wish to reduce the size
    // of your final executable, you should remove from the following
    // the specific initialization routines you do not need
    // Change the registry key under which our settings are stored
    // TODO: You should modify this string to be something appropriate
    // such as the name of your company or organization
    SetRegistryKey(_T("Local AppWizard-Generated Applications"));

    AfxOleInit();
    BOOL result = help.CreateDispatch(_T("DExplore.AppObj.8.0"));
    help.SetCollection(_T("ms-help://MS.VSCC.v80/"), _T(""));

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
    sa.sin_port             = htons (3836); 
    sa.sin_addr.S_un.S_addr = inet_addr ("127.0.0.1");

    bind(sock, (struct sockaddr*)&sa, sizeof(sa));

    listen(sock, SOMAXCONN);

    while(true) {

        SOCKET read_sock = accept(sock, (struct sockaddr*)&sa, NULL);
        char buff[1024] = {0};
        recv(read_sock, buff, 1024, 0);
        BHJDEBUG("received %s", buff);
        fflush(stdout);
        closesocket(read_sock);
        wchar_t wbuff[1024]={0};
        mbstowcs(wbuff, buff, 1024);

        help.DisplayTopicFromF1Keyword(wbuff);

    }
}
