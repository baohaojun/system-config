
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

u32 PASCAL ProcessKey(			// this key will cause the IME go to what state
						  WORD kbd_char,
						  u32 vk,
						  u32 kbd_scan,
						  LPBYTE lpbKeyState, input_context& ic,
						  LPPRIVCONTEXT imcPrivPtr)
{

	if (!ic) {
		return (CST_INVALID);
	}

	if (!imcPrivPtr) {
		return (CST_INVALID);
	}

	// filter system key (alt,alt+,ctrl,shift)
	// and fOpen, IME_CMODE_NOCONVERSION
	if (vk == VK_MENU) {	// ALT key
		return (CST_INVALID);
	} else if (kbd_scan & KF_ALTDOWN) {	// ALT-xx key
		return (CST_INVALID);
	} else if (vk == VK_CONTROL) {	// CTRL key
		return (CST_INVALID);
	} else if (vk == VK_SHIFT) {	// SHIFT key
		return (CST_INVALID);
	} else if (!ic->fOpen) {	// don't compose in 
		// close status
		return (CST_INVALID);
	} else if (ic->fdwConversion & IME_CMODE_NOCONVERSION) {
		// Caps on/off
		if (vk == VK_CAPITAL) {
			return (CST_CAPITAL);
		} else
			return (CST_INVALID);

	} else if (vk >= VK_NUMPAD0 && vk <= VK_DIVIDE) {
		return (CST_INVALID);
	} else {
	}

	// Caps on/off
	if (vk == VK_CAPITAL) {
		return (CST_CAPITAL);
	}
	// candidate alaredy open,  <,>,pageup,pagedown,?,ECS,key
	if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
		if (vk == VK_PRIOR) {	// PageUp
			return (CST_CHOOSE);
		} else if (vk == VK_NEXT) {	// PageDown
			return (CST_CHOOSE);
		} else if (kbd_char == TEXT('-')) {
			return (CST_CHOOSE);
		} else if (kbd_char == TEXT('=')) {
			return (CST_CHOOSE);
		} else if (vk == VK_HOME) {
			return (CST_CHOOSE);
		} else if (vk == VK_END) {
			return (CST_CHOOSE);
		} else if (vk == VK_ESCAPE) {	// Esc
			return (CST_CHOOSE);
		} else if (kbd_char == TEXT(' ')) {
			return (CST_CHOOSE);
		} else {
		}
	}

	// candidate alaredy open, shift + num key
	if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
		if ((kbd_char >= TEXT('0')) && kbd_char <= TEXT('9')) {
			return (CST_CHOOSE);
		}
	}
	// 0
	if (ic->fdwConversion & 0) {	//Code Input Mode
		return (CST_INVALID);
	}

	if (!(ic->fdwConversion & IME_CMODE_NATIVE)) {
		// alphanumeric mode
		if (kbd_char >= TEXT(' ') && kbd_char <= TEXT('~')) {
			return (CST_ALPHANUMERIC);
		} else {
			return (CST_INVALID);
		}
	} else if (kbd_char == TEXT('?')) {
	} else if (kbd_char == TEXT(' ')) {
	} else if (kbd_char >= TEXT(' ') && kbd_char <= TEXT('~')) {
		if (!IsUsedCode(kbd_char) && imcPrivPtr->iImeState != CST_INIT)
			return (CST_INVALID_INPUT);
	}
	// Esc key
	if (vk == VK_ESCAPE) {
		register LPGUIDELINE lpGuideLine;
		register u32 iImeState;

		lpGuideLine = (LPGUIDELINE) ImmLockIMCC(ic->hGuideLine);
		if (!lpGuideLine) {
			return (CST_INVALID);
		}
		if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_START) {
			iImeState = CST_INPUT;
		} else if (!lpGuideLine) {
			iImeState = CST_INVALID;
		} else if (lpGuideLine->dwLevel == GL_LEVEL_NOGUIDELINE) {
			iImeState = CST_INVALID;
		} else {
			// need this key to clean information string or guideline state
			iImeState = CST_INPUT;
		}

		ImmUnlockIMCC(ic->hGuideLine);

		return (iImeState);
	}
	// BackSpace Key
	else if (vk == VK_BACK) {
		if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_START) {
			return (CST_INPUT);
		} else {
			return (CST_INVALID);
		}
	}
	// NumPad key and Other Input vailid key
	else if (vk >= VK_NUMPAD0 && vk <= VK_DIVIDE) {
		return (CST_ALPHANUMERIC);
	} else if (kbd_char > TEXT('~')) {
		return (CST_INVALID);
	} else if (kbd_char < TEXT(' ')) {
		return (CST_INVALID);
	} else if (ic->fdwConversion & IME_CMODE_EUDC) {
	} else {
	}
	if (ic->fdwConversion & IME_CMODE_NATIVE) {
		return (UnicodeProcessKey(kbd_char, imcPrivPtr));
	}

	return (CST_INVALID);
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
