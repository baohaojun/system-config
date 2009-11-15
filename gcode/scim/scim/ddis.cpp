#include <windows.h>
#include <windowsx.h>
#include <winerror.h>
#include <immdev.h>
#include <imedefs.h>
#include <resource.h>
#include <regstr.h>
#include <winuser.h>
#include "imewnd.h"

BOOL WINAPI
ImeInquire(LPIMEINFO lpImeInfo, LPTSTR lpszWndCls, DWORD dwSystemInfoFlags)
{
	if (!lpImeInfo) {
		return FALSE;
	}

	lpImeInfo->dwPrivateDataSize = 0;
	lpImeInfo->fdwProperty = IME_PROP_KBD_CHAR_FIRST | IME_PROP_UNICODE | IME_PROP_IGNORE_UPKEYS | IME_PROP_SPECIAL_UI;

	lpImeInfo->fdwConversionCaps =
		IME_CMODE_NATIVE | IME_CMODE_NOCONVERSION;
	lpImeInfo->fdwSentenceCaps = 0;
	lpImeInfo->fdwUICaps = UI_CAP_ROT90;
	lpImeInfo->fdwSCSCaps = SCS_CAP_COMPSTR | SCS_CAP_MAKEREAD;
	lpImeInfo->fdwSelectCaps = (DWORD) 0;

	lstrcpy(lpszWndCls, (LPTSTR) szUIClassName);
	return (TRUE);
}

BOOL WINAPI ImeConfigure(
	HKL hKL,
	HWND hAppWnd,
	DWORD dwMode,
	LPVOID lpData)
{
	return FALSE;
}

//This function is used by another IME to query what's the 
//encoding of their input character, the hell with it!
DWORD WINAPI
ImeConversionList(HIMC hIMC,
				  LPCTSTR lpszSrc,
				  LPCANDIDATELIST lpCandList, DWORD uBufLen, u32 uFlag)
{
	return 0;
}

BOOL WINAPI ImeDestroy(u32 uReserved)
{
	if (uReserved) {
		return FALSE;
	}

	return (TRUE);
}

//this function is an "escape", meaning that it provides some information that
//can't be get elsewhere. well, we've got no escape!
LRESULT WINAPI ImeEscape(HIMC hIMC, u32 uSubFunc, LPVOID lpData)
{
	return FALSE;

}

BOOL WINAPI ImeSelect(HIMC hIMC, BOOL fSelect)
{
	input_context ic(hIMC, NULL);
	if (!ic) {
		return FALSE;
	}

	return TRUE;
}

BOOL WINAPI ImeSetActiveContext(HIMC hIMC, BOOL fOn)
{
	input_context ic(hIMC, NULL);
	if (!ic) {
		return FALSE;
	}
	return (TRUE);
}

