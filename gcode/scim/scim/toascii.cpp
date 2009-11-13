
/*++

Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

Module Name:

    toascii.c


++*/


#include <windows.h>
#include <immdev.h>
#include <imedefs.h>
#include "imewnd.h"

BOOL IsUsedCode(WORD kbd_char)
{
	WORD wFlg;

	for (wFlg = 0; wFlg < sImeG.wNumCodes; wFlg++)
		if (kbd_char == sImeG.UsedCodes[wFlg])
			break;
	if (wFlg < sImeG.wNumCodes)
		return (TRUE);
	return FALSE;
}

u32 PASCAL UnicodeProcessKey(WORD kbd_char, LPPRIVCONTEXT imcPrivPtr)
{
	if (!imcPrivPtr) {
		return (CST_INVALID);
	}

	if (kbd_char == TEXT(' ')) {
		if (imcPrivPtr->bSeq[0] && imcPrivPtr->bSeq[1]) {
			return (CST_INPUT);
		} else if (!imcPrivPtr->bSeq[0]) {
			return (CST_ALPHANUMERIC);
		} else {
			return (CST_INVALID_INPUT);
		}
	}

	if ((kbd_char >= TEXT('0') && kbd_char <= TEXT('9'))
		|| (kbd_char >= TEXT('a') && kbd_char <= TEXT('f'))
		|| (kbd_char == TEXT('?'))) {

		if (kbd_char == TEXT('?')) {
			if (!imcPrivPtr->bSeq[2]) {
				return (CST_INPUT);
			} else
				return (CST_INVALID_INPUT);
		} else {
			return (CST_INPUT);
		}
	} else if (imcPrivPtr->bSeq[0]) {
		return (CST_INVALID_INPUT);
	} else
		return (CST_ALPHANUMERIC);

}

//this function is called to determing whether the key event is wanted by our IME
//The IME will do the conversion with ImeToAsciiEx
BOOL WINAPI ImeProcessKey(HIMC hIMC,
							 u32 vk, LPARAM lParam,
							 CONST LPBYTE lpbKeyState)
{
	BHJDEBUG(" vk is %x, lParam is %x", vk, lParam);
	char kbd_char = LOBYTE(vk);
	BHJDEBUG(" kbd_char is %x", kbd_char);

	if (isprint(kbd_char)) {
		return true;
	}

	return false;
}


static u32 to_wm_char(
	u32 vk,
	u32 kbd_scan, 
	LPTRANSMSGLIST lpTransBuf, 
	PBYTE kbd_state
	)
{

	BYTE szAscii[4];

	int nChars = ToAscii(vk, kbd_scan, kbd_state,
					 (LPWORD) szAscii, 0);

	if (!nChars) {
		BHJDEBUG(" ToAscii failed");
	}

	LPTRANSMSG lpTransMsg = lpTransBuf->TransMsg;
	int i;
	for (i=0; i<nChars; i++) {

		lpTransMsg[i].message = WM_CHAR;
		lpTransMsg[i].wParam = szAscii[i];
		lpTransMsg[i].lParam = (kbd_scan << 16) | 1UL;
	}
	return i;
}

u32 WINAPI
ImeToAsciiEx(u32 vk,
			 u32 kbd_scan,
			 CONST LPBYTE lpbKeyState,
			 LPTRANSMSGLIST lpTransBuf, u32 fuState, HIMC hIMC)
{
	char kbd_char = (char)(vk);
	BHJDEBUG(" char is %x", vk);

	if (isalpha(kbd_char)) {
		g_comp_str.push_back((char)tolower(kbd_char));
	}

	input_context ic(hIMC, lpTransBuf->TransMsg, lpTransBuf->uMsgCount);

	if (!ic) {
		return to_wm_char(vk, kbd_scan, lpTransBuf, lpbKeyState);
	}

	ic.add_msg(WM_IME_STARTCOMPOSITION, 0, 0);
	ic.add_msg(WM_IME_COMPOSITION, 0, 0);
	int n = 10;
	int i;
	for (i=0; i<n; i++) {
		if (!ic.add_msg(WM_CHAR, 0x9999+i, 1)) {
			break;
		}
	}
	return i;
}
