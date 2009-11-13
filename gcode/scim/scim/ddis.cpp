
/*++

Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

Module Name:

    ddis.c

++*/


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
	lpImeInfo->fdwProperty = IME_PROP_KBD_CHAR_FIRST |
		IME_PROP_UNICODE |
		IME_PROP_CANDLIST_START_FROM_1 |
		IME_PROP_IGNORE_UPKEYS;

	lpImeInfo->fdwConversionCaps =
		IME_CMODE_NATIVE | IME_CMODE_NOCONVERSION;
	lpImeInfo->fdwSentenceCaps = 0;
	// IME will have different distance base multiple of 900 escapement
	lpImeInfo->fdwUICaps = UI_CAP_ROT90;
	// composition string is the reading string for simple IME
	lpImeInfo->fdwSCSCaps = SCS_CAP_COMPSTR | SCS_CAP_MAKEREAD;
	// IME want to decide conversion mode on ImeSelect
	lpImeInfo->fdwSelectCaps = (DWORD) 0;

	lstrcpy(lpszWndCls, (LPTSTR) szUIClassName);

	if (lpImeL) {
		if (dwSystemInfoFlags & IME_SYSINFO_WINLOGON) {
			//  the client app is running in logon mode.
			lpImeL->fWinLogon = TRUE;
		} else
			lpImeL->fWinLogon = FALSE;

	}

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

BOOL PASCAL Select(input_context& ic, BOOL fSelect)
{
	if (fSelect) {
		if (!(ic->fdwInit & INIT_CONVERSION)) {
			ic->fdwConversion = IME_CMODE_NATIVE;
			ic->fdwInit |= INIT_CONVERSION;
		}

		if (!(ic->fdwInit & INIT_LOGFONT)) {
			HDC hDC;
			HGDIOBJ hSysFont;

			//hSysFont = GetStockObject(SYSTEM_FONT);
			hDC = GetDC(NULL);
			hSysFont = GetCurrentObject(hDC, OBJ_FONT);
			GetObject(hSysFont, sizeof(LOGFONT), &ic->lfFont.A);
			ReleaseDC(NULL, hDC);

			ic->fdwInit |= INIT_LOGFONT;
		}


		{
			DWORD fdwConversion;

			if (GetKeyState(VK_CAPITAL) & 0x01) {

				//
				// Change to alphanumeric mode.
				//
				fdwConversion = ic->fdwConversion & ~IME_CMODE_NATIVE;
			} else {

				//
				// Change to native mode
				//
				fdwConversion = ic->fdwConversion | IME_CMODE_NATIVE;
			}

			ImmSetConversionStatus(ic.get_handle(), fdwConversion,
								   ic->fdwSentence);
		}

	}


	return (TRUE);
}

BOOL WINAPI ImeSelect(HIMC hIMC, BOOL fSelect)
{
	input_context ic(hIMC, NULL);
	if (!ic) {
		return FALSE;
	}

	return Select(ic, fSelect);
}

BOOL WINAPI ImeSetActiveContext(HIMC hIMC, BOOL fOn)
{
	input_context ic(hIMC, NULL);
	if (!ic) {
		return FALSE;
	}
	return (TRUE);
}

