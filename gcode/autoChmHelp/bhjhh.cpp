// bhjhh.cpp : Defines the entry point for the application.
//

#include "stdafx.h"
#include "bhjhh.h"
#include "htmlhelp.h"
#include <stdio.h>
#include "bhjdebug.h"
#include <assert.h>
#define MAX_LOADSTRING 100

// Global Variables:
HINSTANCE hInst;								// current instance
TCHAR szTitle[MAX_LOADSTRING];					// The title bar text
TCHAR szWindowClass[MAX_LOADSTRING];			// the main window class name

// Forward declarations of functions included in this code module:
ATOM				MyRegisterClass(HINSTANCE hInstance);
BOOL				InitInstance(HINSTANCE, int);
LRESULT CALLBACK	WndProc(HWND, UINT, WPARAM, LPARAM);
INT_PTR CALLBACK	About(HWND, UINT, WPARAM, LPARAM);

DWORD WINAPI HelpThread(LPVOID) 
{
    char buff[1024]="ImeNotify";
    BHJDEBUG("hello world");
    while (fgets(buff, 1024, stdin)) {
        BHJDEBUG("hello world");
        /*typedef struct tagHH_FTS_QUERY
          {
          int      cbStruct;
          BOOL     fUniCodeStrings;
          LPCTSTR  pszSearchQuery;
          LONG     iProximity;
          BOOL     fStemmedSearch;
          BOOL     fTitleOnly;
          BOOL     fExecute;
          LPCTSTR  pszWindow;
          } HH_FTS_QUERY;
        */
            
        wchar_t wbuff[1024];
        mbstowcs(wbuff, buff, 1024);
        BHJDEBUGW(L"wbuff is %s", wbuff);

        HH_FTS_QUERY q ;
        q.cbStruct = sizeof(q);
        q.fUniCodeStrings = TRUE;
        q.pszSearchQuery = L"ImeSelect";
        q.iProximity = HH_FTS_DEFAULT_PROXIMITY;
        q.fStemmedSearch = FALSE;
        q.fTitleOnly = FALSE;
        q.fExecute = TRUE;
        q.pszWindow = L"Mainwin";
            
        HWND hwndHelp = HtmlHelp(
            GetDesktopWindow(),
            L"c:\\Help.chm",
            HH_DISPLAY_SEARCH,
            (DWORD)&q) ;
        HWND hPaneWnd, hPaneLast, hTabWnd, hDlgWnd, hCtlWnd;

        hPaneWnd = FindWindowEx(hwndHelp, NULL, _T("HH Child"), NULL);
        for (;;) // last HH Child
        {
            hPaneLast = FindWindowEx(hwndHelp, hPaneWnd, _T("HH Child"), NULL); // last HH Child
            if (!hPaneLast)
                break;
            hPaneWnd = hPaneLast;
        }
        BHJDEBUG("hPaneWnd == %x", hPaneWnd);

        hCtlWnd = FindWindowEx(hPaneWnd, NULL, _T("Button"), NULL); // skip Tab Control
        //
        // There are two types of interfaces:
        //
        // 1.
        // Window hierarchy:
        // + Main window 
        //   + HH Child
        //     + Browser ...
        //   + HH Child		  <- second "HH Child" window
        //         - Edit     <- we have to fill this edit
        //         - Buttons  <- and press this buttons
        //         ...
        if (hCtlWnd)
        {
            hCtlWnd = FindWindowEx(hPaneWnd, NULL, _T("Edit"), NULL); // skip Tab Control
            // Set window text
            assert(hCtlWnd != NULL);
            ::SendMessage(hCtlWnd, WM_SETTEXT, 0, (LPARAM)wbuff);	// fill it by our query

            ::SendMessage(hwndHelp, WM_COMMAND, 0xbc7, 0); // 0x3ee -- 'List Topics' button, it runs search
			
            ::SendMessage(hwndHelp, WM_COMMAND, 0xbbe, 0); // 0x3f1 -- 'Display' button, it shows first item
        }
        //2.
        // Window hierarchy:
        // + Main window 
        //   + HH Child
        //     + Browser ...
        //   + HH Child		  <- second "HH Child" window
        //     + Tab Control
        //       + Dialog
        //         - Combobox <- we have to fill this combo
        //         - Buttons  <- and press this buttons
        //         ...
        else
        {
			
            hTabWnd = FindWindowEx(hPaneWnd, NULL, _T("SysTabControl32"), NULL); // skip Tab Control
            hDlgWnd = FindWindowEx(hTabWnd, NULL, NULL, NULL); // skip dialog

            TCHAR szClass[64];
            GetClassName(hDlgWnd, szClass, sizeof(szClass));
            BHJDEBUG("hDlgWnd(1) == %x", hDlgWnd);
            if (!wcsstr(szClass, L"#")) // Is it dialog?
                hDlgWnd = FindWindowEx(hTabWnd, hDlgWnd, NULL, NULL); // skip dialog
            hCtlWnd = FindWindowEx(hDlgWnd, NULL, _T("ComboBox"), NULL); // well, it's combobox

            // Set window text
            assert(hCtlWnd != NULL);
            ::SendMessage(hCtlWnd, WM_SETTEXT, 0, (LPARAM)wbuff);	// fill it by our query
			
            //
            // Run search and show first finded page
            //
            ::SendMessage(hwndHelp, WM_COMMAND, 0x3ee, 0); // 0x3ee -- 'List Topics' button, it runs search
            ::SendMessage(hwndHelp, WM_COMMAND, 0x3f1, 0); // 0x3f1 -- 'Display' button, it shows first item
        }
	
    }
    return(0);
}

int APIENTRY _tWinMain(HINSTANCE hInstance,
                       HINSTANCE hPrevInstance,
                       LPTSTR    lpCmdLine,
                       int       nCmdShow)
{
    BHJDEBUG("hello world");
    UNREFERENCED_PARAMETER(hPrevInstance);
    UNREFERENCED_PARAMETER(lpCmdLine);

    HWND hwnd =
        HtmlHelp(
            GetDesktopWindow(),
            L"c:\\Help.chm>Mainwin",
            HH_DISPLAY_TOPIC,
            NULL) ;

    HANDLE g_hThreadPv = CreateThread(NULL, 0, HelpThread, NULL, 0, NULL);

    // TODO: Place code here.
    MSG msg;
    HACCEL hAccelTable;

    // Initialize global strings
    LoadString(hInstance, IDS_APP_TITLE, szTitle, MAX_LOADSTRING);
    LoadString(hInstance, IDC_BHJHH, szWindowClass, MAX_LOADSTRING);
    MyRegisterClass(hInstance);

    // Perform application initialization:
    if (!InitInstance (hInstance, nCmdShow))
    {
        return FALSE;
    }

    hAccelTable = LoadAccelerators(hInstance, MAKEINTRESOURCE(IDC_BHJHH));

    // Main message loop:
    while (GetMessage(&msg, NULL, 0, 0))
    {
        if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg))
        {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }

    return (int) msg.wParam;
}





//
//  FUNCTION: MyRegisterClass()
//
//  PURPOSE: Registers the window class.
//
//  COMMENTS:
//
//    This function and its usage are only necessary if you want this code
//    to be compatible with Win32 systems prior to the 'RegisterClassEx'
//    function that was added to Windows 95. It is important to call this function
//    so that the application will get 'well formed' small icons associated
//    with it.
//
ATOM MyRegisterClass(HINSTANCE hInstance)
{
    WNDCLASSEX wcex;

    wcex.cbSize = sizeof(WNDCLASSEX);

    wcex.style			= CS_HREDRAW | CS_VREDRAW;
    wcex.lpfnWndProc	= WndProc;
    wcex.cbClsExtra		= 0;
    wcex.cbWndExtra		= 0;
    wcex.hInstance		= hInstance;
    wcex.hIcon			= LoadIcon(hInstance, MAKEINTRESOURCE(IDI_BHJHH));
    wcex.hCursor		= LoadCursor(NULL, IDC_ARROW);
    wcex.hbrBackground	= (HBRUSH)(COLOR_WINDOW+1);
    wcex.lpszMenuName	= MAKEINTRESOURCE(IDC_BHJHH);
    wcex.lpszClassName	= szWindowClass;
    wcex.hIconSm		= LoadIcon(wcex.hInstance, MAKEINTRESOURCE(IDI_SMALL));

    return RegisterClassEx(&wcex);
}

//
//   FUNCTION: InitInstance(HINSTANCE, int)
//
//   PURPOSE: Saves instance handle and creates main window
//
//   COMMENTS:
//
//        In this function, we save the instance handle in a global variable and
//        create and display the main program window.
//
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
    HWND hWnd;

    hInst = hInstance; // Store instance handle in our global variable

    hWnd = CreateWindow(szWindowClass, szTitle, WS_OVERLAPPEDWINDOW,
                        CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, HWND_MESSAGE, NULL, hInstance, NULL);

    if (!hWnd)
    {
        return FALSE;
    }

    ShowWindow(hWnd, nCmdShow);
    UpdateWindow(hWnd);

    return TRUE;
}

//
//  FUNCTION: WndProc(HWND, UINT, WPARAM, LPARAM)
//
//  PURPOSE:  Processes messages for the main window.
//
//  WM_COMMAND	- process the application menu
//  WM_PAINT	- Paint the main window
//  WM_DESTROY	- post a quit message and return
//
//
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    int wmId, wmEvent;
    PAINTSTRUCT ps;
    HDC hdc;

    switch (message)
    {
    case WM_COMMAND:
        wmId    = LOWORD(wParam);
        wmEvent = HIWORD(wParam);
        // Parse the menu selections:
        switch (wmId)
        {
        case IDM_ABOUT:
            DialogBox(hInst, MAKEINTRESOURCE(IDD_ABOUTBOX), hWnd, About);
            break;
        case IDM_EXIT:
            DestroyWindow(hWnd);
            break;
        default:
            return DefWindowProc(hWnd, message, wParam, lParam);
        }
        break;
    case WM_PAINT:
        hdc = BeginPaint(hWnd, &ps);
        // TODO: Add any drawing code here...
        EndPaint(hWnd, &ps);
        break;
    case WM_DESTROY:
        PostQuitMessage(0);
        break;
    default:
        return DefWindowProc(hWnd, message, wParam, lParam);
    }
    return 0;
}

// Message handler for about box.
INT_PTR CALLBACK About(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
    UNREFERENCED_PARAMETER(lParam);
    switch (message)
    {
    case WM_INITDIALOG:
        return (INT_PTR)TRUE;

    case WM_COMMAND:
        if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL)
        {
            EndDialog(hDlg, LOWORD(wParam));
            return (INT_PTR)TRUE;
        }
        break;
    }
    return (INT_PTR)FALSE;
}
