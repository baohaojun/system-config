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

	HMODULE h = LoadLibrary("cygwin1.dll");
	void (*init)() = (void (*)())GetProcAddress(h, "cygwin_dll_init");
	init();

	typedef int ssize_t;
	typedef ssize_t (*ccp_func) (cygwin_conv_path_t what, const void * from, void * to, size_t size);

	ccp_func ccp = (ccp_func)GetProcAddress(h, "cygwin_conv_path");
	char buff[1024] = {0};
	BHJDEBUG(" proc is %d", ccp(0, ".", buff, 1000));
	BHJDEBUG(" buff is %s", buff);
	
	return 0;
	using std::string;
   string TestString = "1111122222333334444455555";
   TestString.resize(50);
   
   const char *dood = "hello world";
   copy(dood, dood+strlen(dood), TestString.begin());
   cout << "[" << TestString << "]" << endl
        << "size: " << TestString.size() << endl
        << endl;

   TestString.resize(5);
   cout << "[" << TestString << "]" << endl
        << "size: " << TestString.size() << endl
        << endl;

   TestString.resize(10);
   cout << "[" << TestString << "]" << endl
        << "size: " << TestString.size() << endl
        << endl;

   TestString.resize(15,'6');
   cout << "[" << TestString << "]" << endl
        << "size: " << TestString.size() << endl;

	return 0;
	cstring str = GetCommandLine();
	BHJDEBUG(" cmdline is `%s'", str.c_str());

	printf("argv ");
	for (int i=0; i<argc; i++) {
		printf("`%s' ", argv[i]);
	}
	printf("\n");

	printf("args ");
	lstring_t ls = cmdline2args(str);

	cmdline_parser cp(str);
	for (lstring_t::iterator i = ls.begin(); i != ls.end(); i++) {
		printf("`%s' ", i->c_str());
	}
	printf("\n");

	for (int i=0; i<ls.size(); i++) {
		printf("text `%s'\n", cp.get_text_of_args(i).c_str());
	}

	return 0;
	print_sh_folders();
	return 0;
	BHJDEBUG(" dirname is %s", bce_dirname("q:\\").c_str());
	BHJDEBUG(" basename is `%s'", basename("/").c_str());
	
	WIN32_FIND_DATA wfd;
	HANDLE hfile = FindFirstFile("c:/***", &wfd);
	while (hfile != INVALID_HANDLE_VALUE) {
		BHJDEBUG(" found a file %s", wfd.cFileName);
		if (FindNextFile(hfile, &wfd) == 0) {
			break;
		}
	}

	return nRetCode;
}
