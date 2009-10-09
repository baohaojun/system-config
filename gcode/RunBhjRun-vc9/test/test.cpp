// test.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "test.h"
#include "../bhjlib.h"
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 
#include <algorithm>
typedef unsigned int cygwin_conv_path_t;
using std::copy;
using namespace bhj;

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// The one and only application object

CWinApp theApp;

using namespace std;

void print_sh_folders()
{
	struct {	
		int csid; 
		const char* name;
	} map [] = {
		{CSIDL_ADMINTOOLS, "CSIDL_ADMINTOOLS"},
		{CSIDL_COMMON_ADMINTOOLS, "CSIDL_COMMON_ADMINTOOLS"},
		{CSIDL_APPDATA, "CSIDL_APPDATA"},
		{CSIDL_COMMON_APPDATA, "CSIDL_COMMON_APPDATA"},
		{CSIDL_COMMON_DOCUMENTS, "CSIDL_COMMON_DOCUMENTS"},
		{CSIDL_COOKIES, "CSIDL_COOKIES"},
		{CSIDL_FLAG_CREATE, "CSIDL_FLAG_CREATE"},
		{CSIDL_HISTORY, "CSIDL_HISTORY"},
		{CSIDL_INTERNET_CACHE, "CSIDL_INTERNET_CACHE"},
		{CSIDL_LOCAL_APPDATA, "CSIDL_LOCAL_APPDATA"},
		{CSIDL_MYPICTURES, "CSIDL_MYPICTURES"},
		{CSIDL_PERSONAL, "CSIDL_PERSONAL"},
		{CSIDL_PROGRAM_FILES, "CSIDL_PROGRAM_FILES"},
		{CSIDL_PROGRAM_FILES_COMMON, "CSIDL_PROGRAM_FILES_COMMON"},
		{CSIDL_SYSTEM, "CSIDL_SYSTEM"},
		{CSIDL_WINDOWS, "CSIDL_WINDOWS"},
		{CSIDL_DESKTOP, "CSIDL_DESKTOP"},
		{0, NULL},
	};
	for (int i=0; map[i].name; i++) {
		char strAppPath[MAX_PATH] = "";
		HRESULT ret = SHGetFolderPath(NULL, map[i].csid, NULL, SHGFP_TYPE_CURRENT, strAppPath);
		if (ret != S_OK) {
			BHJDEBUG(" path for %s not available", map[i].name);
		} else {
			BHJDEBUG(" path for %s is %s", map[i].name, strAppPath);
		}
	}
}

static const char *
startError (int err)
{
  switch (err)
    {
    case 0:
      return "The operating system is out of memory or resources.";
    case ERROR_FILE_NOT_FOUND:
      return "The specified file was not found.";
    case ERROR_PATH_NOT_FOUND:
      return "The specified path was not found.";
    case ERROR_BAD_FORMAT:
      return "The .exe file is invalid (non-Win32 .exe or error in "
        ".exe image).";
    case SE_ERR_ACCESSDENIED:
      return "The operating system denied access to the specified file.";
    case SE_ERR_ASSOCINCOMPLETE:
      return "The file name association is incomplete or invalid.";
    case SE_ERR_DDEBUSY:
      return "The DDE transaction could not be completed because "
        "other DDE transactions were being processed.";
    case SE_ERR_DDEFAIL:
      return "The DDE transaction failed.";
    case SE_ERR_DDETIMEOUT:
      return "The DDE transaction could not be completed because the "
        "request timed out.";
    case SE_ERR_DLLNOTFOUND:
      return "The specified dynamic-link library was not found.";
    case SE_ERR_NOASSOC:
      return "There is no application associated with the given file "
        "name extension.";
    case SE_ERR_OOM:
      return "There was not enough memory to complete the operation.";
    case SE_ERR_SHARE:
      return "A sharing violation occurred.";
    default:
      return "An unknown error occurred.";
    }
}

int _tmain(int argc, TCHAR* argv[], TCHAR* envp[])
{
	int nRetCode = 0;

	// initialize MFC and print and error on failure
	if (!AfxWinInit(::GetModuleHandle(NULL), NULL, ::GetCommandLine(), 0))
	{
		// TODO: change error code to suit your needs
		cerr << _T("Fatal Error: MFC initialization failed") << endl;
		nRetCode = 1;
	}
	else
	{
		// TODO: code your application's behavior here.
		CString strHello;
		strHello.LoadString(IDS_HELLO);
		cout << (LPCTSTR)strHello << endl;
	}

	WIN32_FIND_DATA wfd;
   	HANDLE hfile = FindFirstFile("\\192.168.1.11\*", &wfd);
   	while (hfile != INVALID_HANDLE_VALUE) {
   		BHJDEBUG(" found a file %s", wfd.cFileName);
   		if (FindNextFile(hfile, &wfd) == 0) {
   			break;
   		}
   	}
	return 0;

	int ret = (int)ShellExecute(NULL, NULL, "C:/WINDOWS/system32/desk.cpl", NULL, NULL, SW_SHOWNORMAL);	
	if (ret <= 32) {
		fmt_messagebox("Error: can't start %s: %s", "C:/windows/system32/desk.cpl", startError(ret));
		return 0;
	}
	return 0;
   // 	HMODULE h = LoadLibrary("cygwin1.dll");
   // 	void (*init)() = (void (*)())GetProcAddress(h, "cygwin_dll_init");
   // 	init();

   // 	typedef int ssize_t;
   // 	typedef ssize_t (*ccp_func) (cygwin_conv_path_t what, const void * from, void * to, size_t size);

   // 	ccp_func ccp = (ccp_func)GetProcAddress(h, "cygwin_conv_path");
   // 	char buff[1024] = {0};
   // 	BHJDEBUG(" proc is %d", ccp(0, ".", buff, 1000));
   // 	BHJDEBUG(" buff is %s", buff);
	
   // 	return 0;
   // 	using std::string;
   // string TestString = "1111122222333334444455555";
   // TestString.resize(50);
   
   // const char *dood = "hello world";
   // copy(dood, dood+strlen(dood), TestString.begin());
   // cout << "[" << TestString << "]" << endl
   //      << "size: " << TestString.size() << endl
   //      << endl;

   // TestString.resize(5);
   // cout << "[" << TestString << "]" << endl
   //      << "size: " << TestString.size() << endl
   //      << endl;

   // TestString.resize(10);
   // cout << "[" << TestString << "]" << endl
   //      << "size: " << TestString.size() << endl
   //      << endl;

   // TestString.resize(15,'6');
   // cout << "[" << TestString << "]" << endl
   //      << "size: " << TestString.size() << endl;

   // 	return 0;
   // 	cstring str = GetCommandLine();
   // 	BHJDEBUG(" cmdline is `%s'", str.c_str());

   // 	printf("argv ");
   // 	for (int i=0; i<argc; i++) {
   // 		printf("`%s' ", argv[i]);
   // 	}
   // 	printf("\n");

   // 	printf("args ");
   // 	lstring_t ls = cmdline2args(str);

   // 	cmdline_parser cp(str);
   // 	for (lstring_t::iterator i = ls.begin(); i != ls.end(); i++) {
   // 		printf("`%s' ", i->c_str());
   // 	}
   // 	printf("\n");

   // 	for (int i=0; i<ls.size(); i++) {
   // 		printf("text `%s'\n", cp.get_text_of_args(i).c_str());
   // 	}

   // 	return 0;
   // 	print_sh_folders();
   // 	return 0;
   // 	BHJDEBUG(" dirname is %s", bce_dirname("q:\\").c_str());
   // 	BHJDEBUG(" basename is `%s'", basename("/").c_str());
	
   // 	WIN32_FIND_DATA wfd;
   // 	HANDLE hfile = FindFirstFile("c:/***", &wfd);
   // 	while (hfile != INVALID_HANDLE_VALUE) {
   // 		BHJDEBUG(" found a file %s", wfd.cFileName);
   // 		if (FindNextFile(hfile, &wfd) == 0) {
   // 			break;
   // 		}
   // 	}

   // 	return nRetCode;
}
