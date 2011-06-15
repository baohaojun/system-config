// ToggleDesktop.cpp : Defines the entry point for the application.
//

#include "stdafx.h"
#include <shldisp.h>
int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
{
 	// TODO: Place code here.
    CoInitialize(0);
    IShellDispatch4 * pdisp=NULL;
    CoCreateInstance(CLSID_Shell,NULL,CLSCTX_ALL,__uuidof(IShellDispatch4),(void **)&pdisp);
    pdisp->ToggleDesktop();
    pdisp->Release();
    CoUninitialize();
	return 0;
}
