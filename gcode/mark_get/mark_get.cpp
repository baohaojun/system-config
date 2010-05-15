// mark_get.cpp : Defines the -*- linux-c -*-entry point for the application.
//

#include "stdafx.h"
#include <shellapi.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

#define RCFILE ".mgrc"

void markwin(int);
void getwin(int);
void Usage(int);

int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
{
    // TODO: Place code here.
    int argc;
    LPWSTR pWCmdLine=GetCommandLineW();
    LPWSTR *argv = CommandLineToArgvW(pWCmdLine, &argc);

    if (argc!=2) Usage(1);
  
    char buf1[1024];
  
    wcstombs(buf1, argv[1], 1024);

    if ( (argv[1][0]!='+') && (argv[1][0]!='-'))
	Usage(2);
    if ( (argv[1][1]<'0') || (argv[1][1]>'9'))
	Usage(3);

    if (argv[1][0] == '+')
	markwin(argv[1][1]-'0');
    else if (argv[1][0] == '-')
	getwin(argv[1][1]-'0');

    return 0;
}

void markwin(int n)
{
    printf("markwin %d\n", n);
    HWND ha[10];
    
    FILE *fp;
    
    memset(ha, 0, sizeof(ha));
    fp = fopen("d:\\" RCFILE, "r+");
	if (!fp)
		fp = fopen("d:\\" RCFILE, "w+");
	if (!fp)
		exit(-1);
    fread(ha, sizeof(HWND), sizeof(ha)/sizeof(HWND), fp);

    fseek(fp, 0, SEEK_SET);

    ha[n] = GetForegroundWindow();
    fwrite(ha, sizeof(HWND), sizeof(ha)/sizeof(HWND), fp);
    fclose(fp);
}

void getwin(int n)
{
    printf("getwin %d\n", n);
    HWND ha[10];
    FILE *fp;

    memset(ha, 0, sizeof(ha));
    fp = fopen("d:\\" RCFILE, "r+");
    fread(ha, sizeof(HWND), sizeof(ha)/sizeof(HWND), fp);
    
    if (IsIconic(ha[n]))
	PostMessage(ha[n], WM_SYSCOMMAND, SC_RESTORE, 0);
    SetForegroundWindow(ha[n]);
    fclose(fp);
}

void Usage(int i)
{
    fprintf(stderr, "Usage %d:\n"
	    "\tmark_get [+-]n\n"
	    "\tn is between 0-9, +n to mark, -n to get\n", i);
    exit(-1);
}
