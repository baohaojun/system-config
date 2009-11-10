
/*++

Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

Module Name:

    toascii.c


++*/


#include <windows.h>
#include <immdev.h>
#include <imedefs.h>

BOOL IsUsedCode(WORD wCharCode)
{
	WORD wFlg;

	for (wFlg = 0; wFlg < sImeG.wNumCodes; wFlg++)
		if (wCharCode == sImeG.UsedCodes[wFlg])
			break;
	if (wFlg < sImeG.wNumCodes)
		return (TRUE);
	return FALSE;
}

UINT PASCAL UnicodeProcessKey(	// this key will cause the IME go to what state
								 WORD wCharCode, LPPRIVCONTEXT imcPrivPtr)
{
	if (!imcPrivPtr) {
		return (CST_INVALID);
	}

	if (wCharCode == TEXT(' ')) {
		if (imcPrivPtr->bSeq[0] && imcPrivPtr->bSeq[1]) {
			return (CST_INPUT);
		} else if (!imcPrivPtr->bSeq[0]) {
			return (CST_ALPHANUMERIC);
		} else {
			return (CST_INVALID_INPUT);
		}
	}
	// check finalize char
	// 0000 -- ffff

	if ((wCharCode >= TEXT('0') && wCharCode <= TEXT('9'))
		|| (wCharCode >= TEXT('a') && wCharCode <= TEXT('f'))
		|| (wCharCode == TEXT('?'))) {

		if (wCharCode == TEXT('?')) {
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

UINT PASCAL ProcessKey(			// this key will cause the IME go to what state
						  WORD wCharCode,
						  UINT uVirtKey,
						  UINT uScanCode,
						  LPBYTE lpbKeyState, LPINPUTCONTEXT lpIMC,
						  LPPRIVCONTEXT imcPrivPtr)
{

	if (!lpIMC) {
		return (CST_INVALID);
	}

	if (!imcPrivPtr) {
		return (CST_INVALID);
	}

	// filter system key (alt,alt+,ctrl,shift)
	// and fOpen, IME_CMODE_NOCONVERSION
	if (uVirtKey == VK_MENU) {	// ALT key
		return (CST_INVALID);
	} else if (uScanCode & KF_ALTDOWN) {	// ALT-xx key
		return (CST_INVALID);
	} else if (uVirtKey == VK_CONTROL) {	// CTRL key
		return (CST_INVALID);
	} else if (uVirtKey == VK_SHIFT) {	// SHIFT key
		return (CST_INVALID);
	} else if (!lpIMC->fOpen) {	// don't compose in 
		// close status
		return (CST_INVALID);
	} else if (lpIMC->fdwConversion & IME_CMODE_NOCONVERSION) {
		// Caps on/off
		if (uVirtKey == VK_CAPITAL) {
			return (CST_CAPITAL);
		} else
			return (CST_INVALID);

	} else if (uVirtKey >= VK_NUMPAD0 && uVirtKey <= VK_DIVIDE) {
		return (CST_INVALID);
	} else {
	}

	// Caps on/off
	if (uVirtKey == VK_CAPITAL) {
		return (CST_CAPITAL);
	}
	// candidate alaredy open,  <,>,pageup,pagedown,?,ECS,key
	if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
		if (uVirtKey == VK_PRIOR) {	// PageUp
			return (CST_CHOOSE);
		} else if (uVirtKey == VK_NEXT) {	// PageDown
			return (CST_CHOOSE);
		} else if (wCharCode == TEXT('-')) {
			return (CST_CHOOSE);
		} else if (wCharCode == TEXT('=')) {
			return (CST_CHOOSE);
		} else if (uVirtKey == VK_HOME) {
			return (CST_CHOOSE);
		} else if (uVirtKey == VK_END) {
			return (CST_CHOOSE);
		} else if (uVirtKey == VK_ESCAPE) {	// Esc
			return (CST_CHOOSE);
		} else if (wCharCode == TEXT(' ')) {
			return (CST_CHOOSE);
		} else {
		}
	}

	// candidate alaredy open, shift + num key
	if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
		if ((wCharCode >= TEXT('0')) && wCharCode <= TEXT('9')) {
			return (CST_CHOOSE);
		}
	}
	// IME_CMODE_CHARCODE
	if (lpIMC->fdwConversion & IME_CMODE_CHARCODE) {	//Code Input Mode
		return (CST_INVALID);
	}

	if (!(lpIMC->fdwConversion & IME_CMODE_NATIVE)) {
		// alphanumeric mode
		if (wCharCode >= TEXT(' ') && wCharCode <= TEXT('~')) {
			return (CST_ALPHANUMERIC);
		} else {
			return (CST_INVALID);
		}
	} else if (wCharCode == TEXT('?')) {
	} else if (wCharCode == TEXT(' ')) {
	} else if (wCharCode >= TEXT(' ') && wCharCode <= TEXT('~')) {
		if (!IsUsedCode(wCharCode) && imcPrivPtr->iImeState != CST_INIT)
			return (CST_INVALID_INPUT);
	}
	// Esc key
	if (uVirtKey == VK_ESCAPE) {
		register LPGUIDELINE lpGuideLine;
		register UINT iImeState;

		lpGuideLine = (LPGUIDELINE) ImmLockIMCC(lpIMC->hGuideLine);
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

		ImmUnlockIMCC(lpIMC->hGuideLine);

		return (iImeState);
	}
	// BackSpace Key
	else if (uVirtKey == VK_BACK) {
		if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_START) {
			return (CST_INPUT);
		} else {
			return (CST_INVALID);
		}
	}
	// NumPad key and Other Input vailid key
	else if (uVirtKey >= VK_NUMPAD0 && uVirtKey <= VK_DIVIDE) {
		return (CST_ALPHANUMERIC);
	} else if (wCharCode > TEXT('~')) {
		return (CST_INVALID);
	} else if (wCharCode < TEXT(' ')) {
		return (CST_INVALID);
	} else if (lpIMC->fdwConversion & IME_CMODE_EUDC) {
	} else {
	}
	if (lpIMC->fdwConversion & IME_CMODE_NATIVE) {
		return (UnicodeProcessKey(wCharCode, imcPrivPtr));
	}

	return (CST_INVALID);
}

BOOL WINAPI ImeProcessKey(HIMC hIMC,
							 UINT uVirtKey, LPARAM lParam,
							 CONST LPBYTE lpbKeyState)
{
	LPINPUTCONTEXT lpIMC;
	LPPRIVCONTEXT imcPrivPtr;
	BYTE szAscii[4];
	int nChars;
	int iRet;
	BOOL fRet;

	// can't compose in NULL hIMC
	if (!hIMC) {
		return FALSE;
	}

	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
	if (!lpIMC) {
		return FALSE;
	}

	imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate);
	if (!imcPrivPtr) {
		ImmUnlockIMC(hIMC);
		return FALSE;
	}

	nChars = ToAscii(uVirtKey, HIWORD(lParam), lpbKeyState,
					 (LPWORD) szAscii, 0);

	if (!nChars) {
		szAscii[0] = 0;
	}

	iRet =
		ProcessKey((WORD) szAscii[0], uVirtKey, HIWORD(lParam),
				   lpbKeyState, lpIMC, imcPrivPtr);
	if (iRet == CST_INVALID) {
		fRet = FALSE;
	} else if ((iRet == CST_INPUT) && (uVirtKey == TEXT('\b'))
			   && (imcPrivPtr->iImeState == CST_INIT)) {
		imcPrivPtr->fdwImeMsg = ((imcPrivPtr->fdwImeMsg | MSG_END_COMPOSITION)
							 & ~(MSG_START_COMPOSITION)) &
			~(MSG_IN_IMETOASCIIEX);

		if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
			ClearCand(lpIMC);
			imcPrivPtr->fdwImeMsg = (imcPrivPtr->fdwImeMsg | MSG_CLOSE_CANDIDATE) &
				~(MSG_OPEN_CANDIDATE);
		}

		GenerateMessage(hIMC, lpIMC, imcPrivPtr);
		fRet = FALSE;
	} else if (uVirtKey == VK_CAPITAL) {
		DWORD fdwConversion;
		if (lpbKeyState[VK_CAPITAL] & 0x01) {
			// change to alphanumeric mode
			fdwConversion = lpIMC->fdwConversion & ~(IME_CMODE_CHARCODE |
													 IME_CMODE_NATIVE |
													 IME_CMODE_EUDC);
			uCaps = 1;
		} else {
			// change to native mode
			fdwConversion = (lpIMC->fdwConversion | IME_CMODE_NATIVE) &
				~(IME_CMODE_CHARCODE | IME_CMODE_EUDC);
			uCaps = 0;
		}
		ImmSetConversionStatus(hIMC, fdwConversion, lpIMC->fdwSentence);
		fRet = FALSE;
	} else if ((iRet == CST_ALPHANUMERIC)
			   && !(lpIMC->fdwConversion & IME_CMODE_FULLSHAPE)
			   && (uVirtKey == VK_SPACE)) {
		fRet = FALSE;
	} else {
		fRet = TRUE;
	}

	ImmUnlockIMCC(lpIMC->hPrivate);
	ImmUnlockIMC(hIMC);

	return (fRet);
}

/**********************************************************************/
/* TranslateSymbolChar()                                              */
/* Return Value:                                                      */
/*      the number of translated chars                                */
/**********************************************************************/
UINT PASCAL
TranslateSymbolChar(LPTRANSMSGLIST lpTransBuf,
					WORD wSymbolCharCode, BOOL SymbolMode)
{
	UINT uRet;
	LPTRANSMSG lpTransMsg;

	uRet = 0;
	lpTransMsg = lpTransBuf->TransMsg;

	// NT need to modify this!
	lpTransMsg->message = WM_CHAR;
	lpTransMsg->wParam = (DWORD) wSymbolCharCode;
	lpTransMsg->lParam = 1UL;
	lpTransMsg++;
	uRet++;

	if (SymbolMode) {
		lpTransMsg = lpTransBuf->TransMsg;

		lpTransMsg->message = WM_CHAR;
		lpTransMsg->wParam = (DWORD) wSymbolCharCode;
		lpTransMsg->lParam = 1UL;
		lpTransMsg++;
		uRet++;
	}

	return (uRet);				// generate two messages
}

UINT PASCAL TranslateToAscii(	// translate the key to WM_CHAR
								// as keyboard driver
								UINT uVirtKey,
								UINT uScanCode, LPTRANSMSGLIST lpTransBuf,
								WORD wCharCode)
{
	LPTRANSMSG lpTransMsg;

	lpTransMsg = lpTransBuf->TransMsg;

	if (wCharCode) {			// one char code
		lpTransMsg->message = WM_CHAR;
		lpTransMsg->wParam = wCharCode;
		lpTransMsg->lParam = (uScanCode << 16) | 1UL;
		return (1);
	}
	// no char code case
	return (0);
}

/**********************************************************************/
/* TranslateImeMessage()                                              */
/* Return Value:                                                      */
/*      the number of translated messages                             */
/**********************************************************************/
UINT PASCAL
TranslateImeMessage(LPTRANSMSGLIST lpTransBuf,
					LPINPUTCONTEXT lpIMC, LPPRIVCONTEXT imcPrivPtr)
{
	UINT uNumMsg;
	UINT i;
	BOOL bLockMsgBuf;
	LPTRANSMSG lpTransMsg;

	uNumMsg = 0;
	bLockMsgBuf = FALSE;

	for (i = 0; i < 2; i++) {
		if (imcPrivPtr->fdwImeMsg & MSG_CLOSE_CANDIDATE) {
			if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
				if (!i) {
					uNumMsg++;
				} else {
					lpTransMsg->message = WM_IME_NOTIFY;
					lpTransMsg->wParam = IMN_CLOSECANDIDATE;
					lpTransMsg->lParam = 0x0001;
					lpTransMsg++;
					imcPrivPtr->fdwImeMsg &= ~(MSG_ALREADY_OPEN);
				}
			}
		}

		if (imcPrivPtr->fdwImeMsg & MSG_END_COMPOSITION) {
			if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_START) {
				if (!i) {
					uNumMsg++;
				} else {
					lpTransMsg->message = WM_IME_ENDCOMPOSITION;
					lpTransMsg->wParam = 0;
					lpTransMsg->lParam = 0;
					lpTransMsg++;
					imcPrivPtr->fdwImeMsg &= ~(MSG_ALREADY_START);
				}
			}
		}

		if (imcPrivPtr->fdwImeMsg & MSG_START_COMPOSITION) {
			if (!(imcPrivPtr->fdwImeMsg & MSG_ALREADY_START)) {
				if (!i) {
					uNumMsg++;
				} else {
					lpTransMsg->message = WM_IME_STARTCOMPOSITION;
					lpTransMsg->wParam = 0;
					lpTransMsg->lParam = 0;
					lpTransMsg++;
					imcPrivPtr->fdwImeMsg |= MSG_ALREADY_START;
				}
			}
		}

		if (imcPrivPtr->fdwImeMsg & MSG_IMN_COMPOSITIONPOS) {
			if (!i) {
				uNumMsg++;
			} else {
				lpTransMsg->message = WM_IME_NOTIFY;
				lpTransMsg->wParam = IMN_SETCOMPOSITIONWINDOW;
				lpTransMsg->lParam = 0;
				lpTransMsg++;
			}
		}

		if (imcPrivPtr->fdwImeMsg & MSG_COMPOSITION) {
			if (!i) {
				uNumMsg++;
			} else {
				lpTransMsg->message = WM_IME_COMPOSITION;
				lpTransMsg->wParam = (DWORD) imcPrivPtr->dwCompChar;
				lpTransMsg->lParam = (DWORD) imcPrivPtr->fdwGcsFlag;
				lpTransMsg++;
			}
		}

		if (imcPrivPtr->fdwImeMsg & MSG_GUIDELINE) {
			if (!i) {
				uNumMsg++;
			} else {
				lpTransMsg->message = WM_IME_NOTIFY;
				lpTransMsg->wParam = IMN_GUIDELINE;
				lpTransMsg->lParam = 0;
				lpTransMsg++;
			}
		}

		if (imcPrivPtr->fdwImeMsg & MSG_OPEN_CANDIDATE) {
			if (!(imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN)) {
				if (!i) {
					uNumMsg++;
				} else {
					lpTransMsg->message = WM_IME_NOTIFY;
					lpTransMsg->wParam = IMN_OPENCANDIDATE;
					lpTransMsg->lParam = 0x0001;
					lpTransMsg++;
					imcPrivPtr->fdwImeMsg |= MSG_ALREADY_OPEN;
				}
			}
		}

		if (imcPrivPtr->fdwImeMsg & MSG_CHANGE_CANDIDATE) {
			if (!i) {
				uNumMsg++;
			} else {
				lpTransMsg->message = WM_IME_NOTIFY;
				lpTransMsg->wParam = IMN_CHANGECANDIDATE;
				lpTransMsg->lParam = 0x0001;
				lpTransMsg++;
			}
		}

		if (imcPrivPtr->fdwImeMsg & MSG_IMN_UPDATE_STATUS) {
			if (!i) {
				uNumMsg++;
			} else {
				lpTransMsg->message = WM_IME_NOTIFY;
				lpTransMsg->wParam = IMN_PRIVATE;
				lpTransMsg->lParam = IMN_PRIVATE_UPDATE_STATUS;
				lpTransMsg++;
			}
		}

		if (imcPrivPtr->fdwImeMsg & MSG_IMN_DESTROYCAND) {
			if (!i) {
				uNumMsg++;
			} else {
				lpTransMsg->message = WM_IME_NOTIFY;
				lpTransMsg->wParam = IMN_PRIVATE;
				lpTransMsg->lParam = IMN_PRIVATE_DESTROYCANDWIN;
				lpTransMsg++;
			}
		}

		if (imcPrivPtr->fdwImeMsg & MSG_BACKSPACE) {
			if (!i) {
				uNumMsg++;
			} else {
				lpTransMsg->message = WM_CHAR;
				lpTransMsg->wParam = TEXT('\b');
				lpTransMsg->lParam = 0x000e;
				lpTransMsg++;
			}
		}

		if (!i) {
			HIMCC hMem;

			if (!uNumMsg) {
				return (uNumMsg);
			}

			if (imcPrivPtr->fdwImeMsg & MSG_IN_IMETOASCIIEX) {
				UINT uNumMsgLimit;

				uNumMsgLimit = lpTransBuf->uMsgCount;

				if (uNumMsg <= uNumMsgLimit) {
					lpTransMsg = lpTransBuf->TransMsg;
					continue;
				}
			}
			// we need to use message buffer
			if (!lpIMC->hMsgBuf) {
				lpIMC->hMsgBuf = ImmCreateIMCC(uNumMsg * sizeof(TRANSMSG));
				lpIMC->dwNumMsgBuf = 0;
			} else if (hMem = ImmReSizeIMCC(lpIMC->hMsgBuf,
											(lpIMC->dwNumMsgBuf +
											 uNumMsg) * sizeof(TRANSMSG)))
			{
				if (hMem != lpIMC->hMsgBuf) {
					ImmDestroyIMCC(lpIMC->hMsgBuf);
					lpIMC->hMsgBuf = hMem;
				}
			} else {
				return (0);
			}

			lpTransMsg = (LPTRANSMSG) ImmLockIMCC(lpIMC->hMsgBuf);
			if (!lpTransMsg) {
				return (0);
			}

			lpTransMsg += lpIMC->dwNumMsgBuf;

			bLockMsgBuf = TRUE;
		} else {
			if (bLockMsgBuf) {
				ImmUnlockIMCC(lpIMC->hMsgBuf);
			}
		}
	}

	return (uNumMsg);
}

/**********************************************************************/
/* ImeToAsciiEx()                                                     */
/* Return Value:                                                      */
/*      the number of translated message                              */
/**********************************************************************/
UINT WINAPI
ImeToAsciiEx(UINT uVirtKey,
			 UINT uScanCode,
			 CONST LPBYTE lpbKeyState,
			 LPTRANSMSGLIST lpTransBuf, UINT fuState, HIMC hIMC)
{
	WORD wCharCode;
	LPINPUTCONTEXT lpIMC;
	LPCOMPOSITIONSTRING lpCompStr;
	LPPRIVCONTEXT imcPrivPtr;
	UINT uNumMsg;
	int iRet;

	wCharCode = HIWORD(uVirtKey);
	uVirtKey = LOBYTE(uVirtKey);

	// hIMC=NULL?
	if (!hIMC) {
		uNumMsg =
			TranslateToAscii(uVirtKey, uScanCode, lpTransBuf, wCharCode);
		return (uNumMsg);
	}
	// get lpIMC
	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);

	if (!lpIMC) {
		uNumMsg =
			TranslateToAscii(uVirtKey, uScanCode, lpTransBuf, wCharCode);
		return (uNumMsg);
	}
	// get imcPrivPtr
	imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate);

	if (!imcPrivPtr) {
		ImmUnlockIMC(hIMC);
		uNumMsg =
			TranslateToAscii(uVirtKey, uScanCode, lpTransBuf, wCharCode);
		return (uNumMsg);
	}
	// get lpCompStr and init
	if (imcPrivPtr->fdwGcsFlag & (GCS_RESULTREAD | GCS_RESULT)) {
		lpCompStr = (LPCOMPOSITIONSTRING) ImmLockIMCC(lpIMC->hCompStr);

		if (lpCompStr) {
			lpCompStr->dwResultStrLen = 0;
		}

		ImmUnlockIMCC(lpIMC->hCompStr);

		imcPrivPtr->fdwGcsFlag = (DWORD) 0;
	}
	// Now all composition realated information already pass to app
	// a brand new start

	// init imcPrivPtr->fdwImeMsg
	imcPrivPtr->fdwImeMsg = imcPrivPtr->fdwImeMsg & (MSG_ALREADY_OPEN |
											 MSG_ALREADY_START) |
		MSG_IN_IMETOASCIIEX;

	// Process Key(wCharCode)
	iRet = ProcessKey(wCharCode, uVirtKey, uScanCode, lpbKeyState, lpIMC,
					  imcPrivPtr);

	// iRet process
	// CST_ALPHANUMERIC
	// CST_SYMBOL


	// CST_ALPHANUMERIC
	if (iRet == CST_ALPHANUMERIC) {
		if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
			imcPrivPtr->fdwImeMsg = (imcPrivPtr->fdwImeMsg | MSG_CLOSE_CANDIDATE) &
				~(MSG_OPEN_CANDIDATE) & ~(MSG_IN_IMETOASCIIEX);
			GenerateMessage(hIMC, lpIMC, imcPrivPtr);
		}

		if (lpIMC->fdwConversion & IME_CMODE_SYMBOL) {
			WORD wSymbolCharCode;
			if (wCharCode == TEXT('.')) {
				wSymbolCharCode = 0x3002;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT(',')) {
				wSymbolCharCode = 0xff0c;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT(';')) {
				wSymbolCharCode = 0xff1b;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT(':')) {
				wSymbolCharCode = 0xff1a;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT('?')) {
				wSymbolCharCode = 0xff1f;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT('!')) {
				wSymbolCharCode = 0xff01;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT('(')) {
				wSymbolCharCode = 0xff08;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT(')')) {
				wSymbolCharCode = 0xff09;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT('\\')) {
				wSymbolCharCode = 0x3001;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT('@')) {
				wSymbolCharCode = 0x00b7;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT('&')) {
				wSymbolCharCode = 0x2014;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT('$')) {
				wSymbolCharCode = 0xffe5;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT('_')) {
				wSymbolCharCode = 0x2014;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode, TRUE);
			} else if (wCharCode == TEXT('^')) {
				wSymbolCharCode = 0x2026;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode, TRUE);
			} else if (wCharCode == TEXT('"')) {
				if (imcPrivPtr->uSYHFlg) {
					wSymbolCharCode = 0x201d;
				} else {
					wSymbolCharCode = 0x201c;

				}
				imcPrivPtr->uSYHFlg ^= 0x00000001;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT('\'')) {
				if (imcPrivPtr->uDYHFlg) {
					wSymbolCharCode = 0x2019;
				} else {
					wSymbolCharCode = 0x2018;
				}
				imcPrivPtr->uDYHFlg ^= 0x00000001;
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT('<')) {
				if (imcPrivPtr->uDSMHFlg) {
					wSymbolCharCode = 0x3008;
					imcPrivPtr->uDSMHCount++;
				} else {
					wSymbolCharCode = 0x300a;
					imcPrivPtr->uDSMHFlg = 0x00000001;
				}
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else if (wCharCode == TEXT('>')) {
				if ((imcPrivPtr->uDSMHFlg) && (imcPrivPtr->uDSMHCount)) {
					wSymbolCharCode = 0x3009;
					imcPrivPtr->uDSMHCount--;
				} else {
					wSymbolCharCode = 0x300b;
					imcPrivPtr->uDSMHFlg = 0x00000000;
				}
				uNumMsg =
					TranslateSymbolChar(lpTransBuf, wSymbolCharCode,
										FALSE);
			} else {
				uNumMsg = TranslateToAscii(uVirtKey, uScanCode, lpTransBuf,
										   wCharCode);
			}
		} else {
			uNumMsg = TranslateToAscii(uVirtKey, uScanCode, lpTransBuf,
									   wCharCode);
		}
	}
	// CST_CHOOSE
	else if (iRet == CST_CHOOSE) {
		LPCANDIDATEINFO lpCandInfo;

		lpCandInfo = (LPCANDIDATEINFO) ImmLockIMCC(lpIMC->hCandInfo);
		if (!lpCandInfo) {
			return (CST_INVALID);
		}

		if (uVirtKey == VK_PRIOR) {
			wCharCode = TEXT('-');
		} else if (uVirtKey == VK_NEXT) {
			wCharCode = TEXT('=');
		} else if (uVirtKey == VK_SPACE) {
			wCharCode = TEXT('1');
		} else if (uVirtKey <= TEXT('9')) {
			// convert shift-0 ... shift-9 to 0 ... 9
			wCharCode = (WORD) uVirtKey;
		} else if (uVirtKey == VK_HOME) {
			wCharCode = 0x24;
		} else if (uVirtKey == VK_END) {
			wCharCode = 0x23;
		} else {
		}

		imcPrivPtr->iImeState = CST_CHOOSE;
		ChooseCand(wCharCode, lpIMC, lpCandInfo, imcPrivPtr);

		ImmUnlockIMCC(lpIMC->hCandInfo);

		uNumMsg = TranslateImeMessage(lpTransBuf, lpIMC, imcPrivPtr);
	}
	// CST_INPUT(IME_CMODE_CHARCODE)
	else if (iRet == CST_INPUT
			 && lpIMC->fdwConversion & IME_CMODE_CHARCODE) {
		uNumMsg =
			TranslateToAscii(uVirtKey, uScanCode, lpTransBuf, wCharCode);
	}
	// CST_INPUT 
	else if (iRet == CST_INPUT) {
		LPGUIDELINE lpGuideLine;

		// get lpCompStr & lpGuideLine
		lpCompStr = (LPCOMPOSITIONSTRING) ImmLockIMCC(lpIMC->hCompStr);
		if (!lpCompStr) {
			return (CST_INVALID);
		}

		lpGuideLine = (LPGUIDELINE) ImmLockIMCC(lpIMC->hGuideLine);
		if (!lpGuideLine) {
			ImmUnlockIMCC(lpIMC->hCompStr);
			return (CST_INVALID);
		}
		// composition
		CompWord(wCharCode, lpIMC, lpCompStr, imcPrivPtr, lpGuideLine);

		ImmUnlockIMCC(lpIMC->hGuideLine);
		ImmUnlockIMCC(lpIMC->hCompStr);

		// generate message
		uNumMsg = TranslateImeMessage(lpTransBuf, lpIMC, imcPrivPtr);
	}
	// ELSE
	else if (iRet == CST_INVALID_INPUT) {
		MessageBeep((UINT) - 1);
		uNumMsg = 0;
	} else {
		uNumMsg =
			TranslateToAscii(uVirtKey, uScanCode, lpTransBuf, wCharCode);
	}

	// reset imcPrivPtr->fdwImeMsg
	imcPrivPtr->fdwImeMsg &= (MSG_ALREADY_OPEN | MSG_ALREADY_START);
	imcPrivPtr->fdwGcsFlag &= (GCS_RESULTREAD | GCS_RESULT);

	ImmUnlockIMCC(lpIMC->hPrivate);
	ImmUnlockIMC(hIMC);

	return (uNumMsg);
}
