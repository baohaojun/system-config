#define UNICODE
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <shellapi.h>
#include <tchar.h>

// type signatures for function pointers
typedef BOOL (WINAPI *PW64DW64FR)(PVOID *);
typedef BOOL (WINAPI *PW64RW64FR)(PVOID);

// scan for the next space not within quotes and replace it with terminator;
// return a pointer to the next character in the string, i.e. what is remnant
// if the original is empty, then return pointer to the empty string
LPTSTR Tokenize(LPTSTR cmdline) {
	if ( *cmdline != '\0' ) {		
		while ( ( *cmdline != ' ' ) && ( *cmdline != '\t' ) && ( *cmdline != '\0' ) ) {
			if ( *cmdline == '\"' ) {
				// skip chars until double-quote or end of string
				do {
					++cmdline;
				} while( ( *cmdline != '\"' ) && ( *cmdline != '\0' ) );
				// if this was a double-quote, skip it
				if( *cmdline == '\"' ) {
					++cmdline;
				}
			}
			else {
				++cmdline;
			}
		}
		// whitespace can be replaced by null to get first token
		if ( *cmdline != '\0' ) {
			*cmdline++ = '\0';
		}
		// trim to next argument by skipping whitespace
		while( ( ( *cmdline == ' ' ) || ( *cmdline == '\t' ) ) && ( *cmdline != '\0' ) ) {
			++cmdline;
		}
	}
	return cmdline;
}

// ZeroMemory replacement to avoid linking to libc (for _memset)
void ZeroFill(IN VOID UNALIGNED *ptr, IN SIZE_T len) {
	char *dst;
	for( dst = (char*) ptr; len > 0; len--, dst++) {
		*dst = 0;
	}
}

// we don't need any runtime initialization; only use Win32 API!
void __cdecl WinMainCRTStartup(void) {
   // variable for ExitProcess
	UINT exitCode;

	// variables for Tokenize
	LPTSTR infName;

	// variables for GetCurrentDirectory
	LPTSTR longDir;
	LPTSTR shortDir;

	// variables for lstrcpy, lstrcat
	DWORD len;
	LPTSTR fixCmd;	
	LPTSTR argList;

	// variables for ShellExecuteEx
	SHELLEXECUTEINFO shExec;

	// variables for Wow64DisableWow64FsRedirection
	PVOID OldWow64FsRedirectionValue;

	// variables for VerifyVersionInfo
	OSVERSIONINFOEX verInfo;

	// declare these functions as pointers to load dynamically
	PW64DW64FR Wow64DisableWow64FsRedirection;
	PW64RW64FR Wow64RevertWow64FsRedirection;

	// attempt to load functions and store pointer in variable
	Wow64DisableWow64FsRedirection = (PW64DW64FR) GetProcAddress(
			GetModuleHandle(TEXT("kernel32.dll")), 
			"Wow64DisableWow64FsRedirection");
	Wow64RevertWow64FsRedirection = (PW64RW64FR) GetProcAddress(
			GetModuleHandle(TEXT("kernel32.dll")), 
			"Wow64RevertWow64FsRedirection");	

	// get the command line buffer from the environment
	infName = Tokenize (GetCommandLine ());

	// standard prefix to run an installer
	fixCmd = TEXT("setupapi.dll,InstallHinfSection DefaultInstall 132 ");

	// get current directory in a short name form
	len = GetCurrentDirectory (0, NULL);
	longDir = (LPTSTR) HeapAlloc (GetProcessHeap (), 0, len * sizeof(TCHAR));
	GetCurrentDirectory (len, longDir);
	len = GetShortPathName (longDir, NULL, 0);
	shortDir = (LPTSTR) HeapAlloc (GetProcessHeap (), 0, len * sizeof(TCHAR));
	GetShortPathName (longDir, shortDir, len);
	
	// put all portions together to a total command line
	len = lstrlen (fixCmd) + lstrlen (shortDir) + lstrlen (TEXT("\\")) + lstrlen (infName);
	argList = (LPTSTR) HeapAlloc (GetProcessHeap (), 0, len * sizeof(TCHAR));
	lstrcpy (argList, fixCmd);
	lstrcat (argList, shortDir);
	lstrcat (argList, TEXT("\\"));
	lstrcat (argList, infName);	
	//MessageBox(NULL, argList, TEXT("argList"), MB_ICONINFORMATION | MB_OK);

	ZeroFill (&shExec, sizeof(SHELLEXECUTEINFO));
	shExec.cbSize = sizeof(SHELLEXECUTEINFO);
	shExec.fMask = SEE_MASK_NOCLOSEPROCESS | SEE_MASK_FLAG_DDEWAIT;
	
	// <http://codefromthe70s.org/vistatutorial.aspx>
	// <http://www.wintellect.com/cs/blogs/jrobbins/archive/2007/03/27/elevate-a-process-at-the-command-line-in-vista.aspx>
	ZeroFill (&verInfo, sizeof(OSVERSIONINFOEX));
	verInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
	verInfo.dwMajorVersion = 6; // Vista
	if (VerifyVersionInfo (&verInfo, VER_MAJORVERSION,
			VerSetConditionMask (0, VER_MAJORVERSION, VER_GREATER_EQUAL))) {
		shExec.lpVerb = TEXT("runas");
	}
	shExec.lpFile = TEXT("rundll32.exe");
	shExec.lpParameters = argList;
	shExec.nShow = SW_SHOWDEFAULT;

	// only call the WoW64 functions if they are available on our system
	if(NULL != Wow64DisableWow64FsRedirection)
		Wow64DisableWow64FsRedirection (&OldWow64FsRedirectionValue);

	// launch process and "inherit" exit code
	ShellExecuteEx (&shExec);
	WaitForSingleObject (shExec.hProcess, INFINITE);
	GetExitCodeProcess (shExec.hProcess, &exitCode);
	CloseHandle (shExec.hProcess);
  
	if (NULL != Wow64RevertWow64FsRedirection)
		Wow64RevertWow64FsRedirection (OldWow64FsRedirectionValue);

	// not really necessary, but it's a habit hard to turn
	HeapFree (GetProcessHeap (), 0, argList);
	HeapFree (GetProcessHeap (), 0, shortDir);
	HeapFree (GetProcessHeap (), 0, longDir);
  
	ExitProcess (exitCode);
}
