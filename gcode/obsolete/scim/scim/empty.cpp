#include <windows.h>
#include <immdev.h>
#include <imedefs.h>

BOOL WINAPI
ImeRegisterWord(LPCTSTR lpszReading, DWORD dwStyle, LPCTSTR lpszString)
{
	return FALSE;
}

BOOL WINAPI
ImeUnregisterWord(LPCTSTR lpszReading, DWORD dwStyle, LPCTSTR lpszString)
{
	return FALSE;
}

u32 WINAPI ImeGetRegisterWordStyle(u32 nItem, LPSTYLEBUF lpStyleBuf)
{
	return FALSE;
}

u32 WINAPI
ImeEnumRegisterWord(REGISTERWORDENUMPROC lpfnRegisterWordEnumProc,
					LPCTSTR lpszReading,
					DWORD dwStyle, LPCTSTR lpszString, LPVOID lpData)
{
	return FALSE;
}

//we don't allow ImeSetCompositionString
//we don't know what it does!

BOOL WINAPI
ImeSetCompositionString(HIMC hIMC,
						DWORD dwIndex,
						LPVOID lpComp,
						DWORD dwCompLen, LPVOID lpRead, DWORD dwReadLen)
{
	return FALSE;
}

// and this one seems useless too!
BOOL WINAPI
NotifyIME(HIMC hIMC, DWORD dwAction, DWORD dwIndex, DWORD dwValue)
{
	return FALSE;
}
