// ActivateDevEnv.cpp -- A little piece of COM crapola to execute a
// command in a running vs.net app.  Useful for staying the hell out
// of the IDE and automating stuff from emacs.
//
// Thatcher Ulrich http://tulrich.com
// Brian Sharon
//
// This program has been donated to the Public Domain.


#define WIN32_LEAN_AND_MEAN
#include <stdio.h>
#include <tchar.h>
#include <string.h>
#include "objbase.h"
#include "windows.h"
#include <assert.h>


#pragma comment( lib, "user32" )
#pragma comment( lib, "ole32" )
#pragma comment( lib, "oleaut32" )

static HWND hDevEnvWindow;

BOOL CALLBACK FindDevenvEnumProc(HWND hwnd, LPARAM lparam);


int main(int argc, char* argv[])
{
	::CoInitialize(NULL);

	// Translate server ProgID into a CLSID. ClsidFromProgID
	// gets this information from the registry.
	CLSID clsid0, clsid1;

	// Try a couple of possibilities.
	CLSIDFromProgID(L"VisualStudio.DTE.9.0", &clsid0);
	CLSIDFromProgID(L"VisualStudio.DTE", &clsid1);

	// Get an interface to the running instance, if any..
	IUnknown *pUnk;
	HRESULT hr = GetActiveObject(clsid0, NULL, (IUnknown**)&pUnk);
	if (hr != 0)
	{
		// Try to find a different version of VS .NET.
		hr = GetActiveObject(clsid1, NULL, (IUnknown**)&pUnk);
		if (hr != 0)
		{
			printf("Can't find a running instance of VisualStudio.DTE\n");
			return 1;
		}
	}

	// Get IDispatch interface for Automation...
	IDispatch *pDisp;
	hr = pUnk->QueryInterface(IID_IDispatch, (void **)&pDisp);
	if (hr != 0)
	{
		printf("QueryInterface failed\n");
		return 1;
	}

	// Release the no-longer-needed IUnknown...
	pUnk->Release();

	// Retrieve the dispatch identifier for the "ExecuteCommand"
	// method.
	DISPID dispid;
	OLECHAR FAR* method = L"ExecuteCommand";
	hr = pDisp->GetIDsOfNames(
		IID_NULL,
		&method,
		1,
		LOCALE_USER_DEFAULT,
		&dispid);

	// Build the command line by concatenating all the args into one string.
	WCHAR arg0[1000];
	arg0[0] = 0;
	for (int i = 1; i < argc; i++)
	{
		size_t	len = wcslen(arg0);
		if (i > 1)
		{
			if (len < sizeof(arg0) / sizeof(arg0[0]) - 1)
			{
				// Insert a space to separate args.
				arg0[len] = ' ';
				len++;
				arg0[len] = 0;
			}
			// else buffer truncate.
		}

		WCHAR*	p = arg0 + len;
		MultiByteToWideChar( CP_ACP, 0, argv[i], -1, p, sizeof(arg0)/sizeof(arg0[0]) - len );
	}

	if (arg0[0] == 0)
	{
		// No command to send!  Bail.
		printf("Send a command-line command to a running instance of Visual Studio .NET\n");
		printf("\n");
		printf("\"Written\" by Thatcher Ulrich http://tulrich.com & Brian Sharon by cutting\n");
		printf("and pasting random snippets from the web and MSDN.\n");
		printf("\n");
		printf("This program has been donated to the Public Domain\n");
		printf("\n");
		printf("usage: ActivateDevEnv command [args ...]\n");
		exit(0);
	}

	VARIANT variant_result;
	VariantInit(&variant_result);
	EXCEPINFO ExceptInfo;

	VARIANTARG variant_arg[1];
	VariantInit(&variant_arg[0]);
	variant_arg[0].vt = VT_BSTR;
	variant_arg[0].bstrVal = SysAllocString(arg0);
	hr = VariantChangeType(&variant_arg[0], &variant_arg[0], 0, VT_BSTR);

	DISPPARAMS args = { variant_arg, NULL, 1, 0};
	hr = pDisp->Invoke(
		dispid,
		IID_NULL,
		LOCALE_SYSTEM_DEFAULT,
		DISPATCH_METHOD,
		&args,
		&variant_result,
		&ExceptInfo,
		NULL);

	::CoUninitialize();

	if (hr != 0)
	{
		// Get an error description using FormatMessage.
		char buf[1000];
		FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | 
					  FORMAT_MESSAGE_IGNORE_INSERTS,
					  NULL, hr, 0, buf, 
					  sizeof(buf) / sizeof(buf[0]), NULL);
		printf(buf);

		// In case of exception, would be nice to print the message.
		// But I can't stomach any more wide-char nonsense right
		// now...

		return 1;
	}

	// Look for the VS.NET main window
	EnumWindows(FindDevenvEnumProc, NULL);

	if (hDevEnvWindow)
	{
		// If we find it, make it active
		::SetForegroundWindow(hDevEnvWindow);
	}
    
	// No error.
	return 0;
}


//********************************************************************
//
// Enumeration routine, called with each child window of the top-level
// window that we hope is VS.NET
//
//********************************************************************
BOOL CALLBACK FindVSTextEditPaneEnumProc(HWND hwnd, LPARAM lparam)
{
    // If we can find a child of this top-level window with the
    // VsTextEditPane class, we're pretty sure we've found the app.
    TCHAR buffer[128];
    ::GetClassName(hwnd, buffer, sizeof(buffer)/sizeof(TCHAR));
    if (_tcscmp(buffer, "VsTextEditPane") == 0)
    {
        // Found it.
        *(bool *)lparam = true;
        // Stop enumerating
        return FALSE;
    }

    // Keep going
    return TRUE;
}


//********************************************************************
//
// Enumeration routine, called with each top-level window on the
// screen.  From within this routine, we will enumerate the child
// windows of any top-level window with the same window class as
// VS.NET.  The child window enumeration routine will use
// FindVSTextEditPaneEnumProc above.
//
//********************************************************************
BOOL CALLBACK FindDevenvEnumProc(HWND hwnd, LPARAM lparam)
{
    // Check the window class to see if we've got a VC top-level
    // window.
    TCHAR buffer[128];
    ::GetClassName(hwnd, buffer, sizeof(buffer)/sizeof(TCHAR));
    if (_tcscmp(buffer, "wndclass_desked_gsk") == 0)
    {
        // printf("Candidate window 0x%08X...", hwnd);

        // Could be VS.net...could be MSDN.  Gotta look at the child
        // windows.
        bool foundIt = false;
        EnumChildWindows(hwnd,
                         FindVSTextEditPaneEnumProc,
                         (LPARAM)&foundIt);

        if (foundIt)
        {
            // printf("looks good!\n");

            hDevEnvWindow = hwnd;

            // Found it; return false to stop enumerating
            return FALSE;
        }
        else
        {
            // printf("no good.  Guessing it's MSDN\n");
        }
    }
    
    // Keep looking...
    return TRUE;
}
