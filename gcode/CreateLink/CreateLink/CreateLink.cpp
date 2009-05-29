// CreateLink.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "CreateLink.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// The one and only application object

CWinApp theApp;

using namespace std;

// CreateLink - uses the Shell's IShellLink and IPersistFile interfaces 
//              to create and store a shortcut to the specified object. 
//
// Returns the result of calling the member functions of the interfaces. 
//
// Parameters:
// lpszPathObj  - address of a buffer containing the path of the object. 
// lpszPathLink - address of a buffer containing the path where the 
//                Shell link is to be stored. 
// lpszDesc     - address of a buffer containing the description of the 
//                Shell link. 

HRESULT CreateLink(LPCTSTR lpszPathObj,  LPCTSTR lpszPathLink, LPCTSTR lpszDesc) 
{ 
	HRESULT hres; 
	IShellLink* psl; 
 
	hres = CoCreateInstance(CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER, 
				IID_IShellLink, (LPVOID*)&psl); 

	if (SUCCEEDED(hres)) 
	{ 
		IPersistFile* ppf; 
 
		psl->SetPath(lpszPathObj); 
		psl->SetDescription(lpszDesc); 
		hres = psl->QueryInterface(IID_IPersistFile, (LPVOID*)&ppf); 
 
		if (SUCCEEDED(hres)) 
		{ 
			hres = ppf->Save(lpszPathLink, TRUE); 
			ppf->Release(); 
		} 
		psl->Release(); 
	} 
	return hres; 
}

int _tmain(int argc, TCHAR* argv[], TCHAR* envp[])
{
	int nRetCode = 0;
	if (argc != 3) {
		printf("Error: usage takes 2 args, src, target\n");
		return -1;
	}

	// initialize MFC and print and error on failure
	if (!AfxWinInit(::GetModuleHandle(NULL), NULL, ::GetCommandLine(), 0))
	{
		// TODO: change error code to suit your needs
		_tprintf(_T("Fatal Error: MFC initialization failed\n"));
		return 1;
	}
	HRESULT res = CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);

	res = CreateLink(argv[1], argv[2], L"");
	if (res != 0) {
		printf("Error: CreateLink failed\n");
		return -1;
	}
	
	return 0;
}
