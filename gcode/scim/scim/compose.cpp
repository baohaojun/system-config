/*++

Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

Module Name:

   compose.c

++*/


#include <windows.h>
#include <immdev.h>
#include <imedefs.h>

void PASCAL UnicodeAddCodeIntoCand(LPCANDIDATELIST, WORD);

WORD PASCAL UnicodeEngine(LPPRIVCONTEXT imcPrivPtr)
{
	if (imcPrivPtr->bSeq[3] || imcPrivPtr->bSeq[2] == TEXT('?')
		|| imcPrivPtr->bSeq[2] == TEXT(' ')) {
		if (imcPrivPtr->bSeq[2] == TEXT('?') || imcPrivPtr->bSeq[2] == TEXT(' ')) {
			imcPrivPtr->bSeq[2] = TEXT('0');
			imcPrivPtr->bSeq[3] = TEXT('0');
		}
		return (AsciiToGB(imcPrivPtr));
	} else {
		return (0);
	}
}

WORD PASCAL AsciiToGB(LPPRIVCONTEXT imcPrivPtr)
{
	WORD GBCode;

	GBCode =
		(CharToHex(imcPrivPtr->bSeq[2]) << 4) + CharToHex(imcPrivPtr->bSeq[3]);
	GBCode = GBCode * 256;
	GBCode =
		(CharToHex(imcPrivPtr->bSeq[0]) << 4) + CharToHex(imcPrivPtr->bSeq[1]) +
		GBCode;

	return (GBCode);
}

WORD PASCAL CharToHex(TCHAR cChar)
{
	if (cChar >= TEXT('0') && cChar <= TEXT('9'))
		return ((WORD) (cChar - TEXT('0')));
	else if (cChar >= TEXT('a') && cChar <= TEXT('f'))
		return ((WORD) (cChar - TEXT('a') + 0x0a));
	else
		return ((WORD) NULL);
}



int PASCAL
Engine(LPCOMPOSITIONSTRING lpCompStr,
	   LPCANDIDATELIST lpCandList,
	   LPPRIVCONTEXT imcPrivPtr, LPINPUTCONTEXT lpIMC, WORD kbd_char)
{
	if (lpCompStr->dwCursorPos < 4
		&& (imcPrivPtr->bSeq[2] != TEXT('?')) && (kbd_char != TEXT(' '))) {
		return (ENGINE_COMP);
	} else if ((lpCompStr->dwCursorPos == 4)
			   || (imcPrivPtr->bSeq[2] == TEXT('?'))
			   || ((kbd_char == TEXT(' '))
				   && (lpCompStr->dwCursorPos == 2))) {

		if (!lpCompStr) {
			MessageBeep((u32) - 1);
			return -1;
		}

		if (!imcPrivPtr) {
			MessageBeep((u32) - 1);
			return -1;
		}

		// UNICODE
		DWORD i;
		WORD wCode;
		TCHAR ResaultStr[3];

		memset(ResaultStr, 0, sizeof(ResaultStr));

		if ((imcPrivPtr->bSeq[2] == TEXT('?') || kbd_char == TEXT(' '))) {
			imcPrivPtr->bSeq[2] = TEXT('0');
			imcPrivPtr->bSeq[3] = TEXT('0');
			imcPrivPtr->bSeq[4] = TEXT('\0');

			wCode = UnicodeEngine(imcPrivPtr);

			wCode = HIBYTE(wCode) | (LOBYTE(wCode) << 8);

			lpCandList->dwCount = 0;
			for (i = 0; i < IME_UNICODE_MAXCAND; i++, wCode++) {
				// add this string into candidate list
				*(LPTSTR) ((LPBYTE) lpCandList +
						   lpCandList->dwOffset[lpCandList->dwCount]) =
					wCode;
				// null terminator
				*(LPTSTR) ((LPBYTE) lpCandList +
						   lpCandList->dwOffset[lpCandList->dwCount] +
						   sizeof(WORD)) = TEXT('\0');

				lpCandList->dwOffset[lpCandList->dwCount + 1] =
					lpCandList->dwOffset[lpCandList->dwCount] +
					sizeof(WORD) + sizeof(TCHAR);
				lpCandList->dwCount++;

			}
			return (ENGINE_COMP);
		} else {
			InitCompStr(lpCompStr);

			// the result string = the selected candidate;
			wCode = UnicodeEngine(imcPrivPtr);
			{
				WCHAR UniStr[2];

				UniStr[0] = HIBYTE(wCode) | (LOBYTE(wCode) << 8);
				UniStr[1] = 0;
				lstrcpy((LPTSTR)
						((LPBYTE) lpCompStr +
						 lpCompStr->dwResultStrOffset), UniStr);

				// calculate result string length
				lpCompStr->dwResultStrLen = lstrlen(UniStr);
			}
			return (ENGINE_RESAULT);
		}

	}
	MessageBeep((u32) - 1);
	return (ENGINE_COMP);
}

/**********************************************************************/
/* AddCodeIntoCand()                                                  */
/**********************************************************************/
void PASCAL AddCodeIntoCand(LPCANDIDATELIST lpCandList, WORD wCode)
{
	WORD wInCode;

	if (lpCandList->dwCount >= IME_MAXCAND) {
		return;
	}

	wInCode = HIBYTE(wCode) | (LOBYTE(wCode) << 8);
	{
		TCHAR wUnicode;
		// change CP_ACP to 936, so that it can work under Multilingul Env.
		MultiByteToWideChar(NATIVE_ANSI_CP, 0, (LPCSTR) & wInCode, 2,
							&wUnicode, 1);
		*(LPUNAWORD) ((LPBYTE) lpCandList +
					  lpCandList->dwOffset[lpCandList->dwCount]) =
			wUnicode;
	}
	// null terminator
	*(LPTSTR) ((LPBYTE) lpCandList +
			   lpCandList->dwOffset[lpCandList->dwCount] + sizeof(WORD)) =
		TEXT('\0');

	lpCandList->dwOffset[lpCandList->dwCount + 1] =
		lpCandList->dwOffset[lpCandList->dwCount] +
		sizeof(WORD) + sizeof(TCHAR);
	lpCandList->dwCount++;
	return;
}

void PASCAL UnicodeAddCodeIntoCand(LPCANDIDATELIST lpCandList, WORD wCode)
{
	if (lpCandList->dwCount >= IME_UNICODE_MAXCAND) {
		return;
	}
	// add this string into candidate list
	*(LPUNAWORD) ((LPBYTE) lpCandList +
				  lpCandList->dwOffset[lpCandList->dwCount]) =
		HIBYTE(wCode) | (LOBYTE(wCode) << 8);

	// null terminator
	*(LPTSTR) ((LPBYTE) lpCandList +
			   lpCandList->dwOffset[lpCandList->dwCount] + sizeof(WORD)) =
		TEXT('\0');

	lpCandList->dwOffset[lpCandList->dwCount + 1] =
		lpCandList->dwOffset[lpCandList->dwCount] +
		sizeof(WORD) + sizeof(TCHAR);
	lpCandList->dwCount++;

	return;
}

void PASCAL
CompEscapeKey(LPINPUTCONTEXT lpIMC,
			  LPCOMPOSITIONSTRING lpCompStr,
			  LPGUIDELINE lpGuideLine, LPPRIVCONTEXT imcPrivPtr)
{

	if (imcPrivPtr->iImeState != CST_CHOOSE) {
		if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_START) {
			imcPrivPtr->fdwImeMsg = (imcPrivPtr->fdwImeMsg | MSG_END_COMPOSITION) &
				~(MSG_START_COMPOSITION);
		}
	}


	imcPrivPtr->iImeState = CST_INIT;
	*(LPDWORD) imcPrivPtr->bSeq = 0;

	if (lpCompStr) {
		InitCompStr(lpCompStr);
		imcPrivPtr->fdwImeMsg |= MSG_COMPOSITION;
		imcPrivPtr->dwCompChar = VK_ESCAPE;
		imcPrivPtr->fdwGcsFlag |= (GCS_COMPREAD | GCS_COMP | GCS_CURSORPOS |
							   GCS_DELTASTART);
	}

	return;
}

void PASCAL
CompBackSpaceKey(LPINPUTCONTEXT lpIMC,
				 LPCOMPOSITIONSTRING lpCompStr, LPPRIVCONTEXT imcPrivPtr)
{

	if (lpCompStr->dwCursorPos < sizeof(BYTE)) {
		lpCompStr->dwCursorPos = sizeof(BYTE);
	}

	imcPrivPtr->bSeq[3] = 0;

	// go back a compsoition char
	lpCompStr->dwCursorPos -= sizeof(BYTE);

	// clean the sequence code
	imcPrivPtr->bSeq[lpCompStr->dwCursorPos] = 0;

	imcPrivPtr->fdwImeMsg |= MSG_COMPOSITION;
	imcPrivPtr->dwCompChar = TEXT('\b');
	imcPrivPtr->fdwGcsFlag |= (GCS_COMPREAD | GCS_COMP | GCS_CURSORPOS |
						   GCS_DELTASTART);

	if (!lpCompStr->dwCursorPos) {
		if (imcPrivPtr->fdwImeMsg & (MSG_ALREADY_OPEN)) {
			ClearCand(lpIMC);
		}

		if (imcPrivPtr->iImeState != CST_INIT) {
			imcPrivPtr->iImeState = CST_INIT;
			lpCompStr->dwCompReadStrLen = lpCompStr->dwCompStrLen =
				lpCompStr->dwDeltaStart = lpCompStr->dwCursorPos;
			Finalize(lpIMC, lpCompStr, imcPrivPtr, TEXT('\b'));
			return;
		}

		if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_START) {
			InitCompStr(lpCompStr);
			imcPrivPtr->fdwImeMsg = (imcPrivPtr->fdwImeMsg | MSG_END_COMPOSITION) &
				~(MSG_START_COMPOSITION);
			return;
		}
	}
	// reading string is composition string for some simple IMEs
	// delta start is the same as cursor position for backspace
	lpCompStr->dwCompReadStrLen = lpCompStr->dwCompStrLen =
		lpCompStr->dwDeltaStart = lpCompStr->dwCursorPos;

	Finalize(lpIMC, lpCompStr, imcPrivPtr, TEXT('\b'));

	return;
}

void PASCAL
CompStrInfo(LPCOMPOSITIONSTRING lpCompStr,
			LPPRIVCONTEXT imcPrivPtr, LPGUIDELINE lpGuideLine, WORD kbd_char)
{
	register DWORD dwCursorPos;

	//
	dwCursorPos = lpCompStr->dwCursorPos;

	// set MSG_START_COMPOSITION
	if (!(imcPrivPtr->fdwImeMsg & MSG_ALREADY_START)) {
		imcPrivPtr->fdwImeMsg = (imcPrivPtr->fdwImeMsg | MSG_START_COMPOSITION) &
			~(MSG_END_COMPOSITION);
	}

	if (imcPrivPtr->iImeState == CST_INIT) {
		// clean the 4 bytes in one time
		*(LPDWORD) imcPrivPtr->bSeq = 0;
	}


	imcPrivPtr->bSeq[dwCursorPos] = (BYTE) kbd_char;

	imcPrivPtr->dwCompChar = (DWORD) kbd_char;

	// set reading string for lpCompStr
	*((LPUNAWORD) ((LPBYTE) lpCompStr + lpCompStr->dwCompReadStrOffset +
				   dwCursorPos * sizeof(TCHAR))) =
		(BYTE) imcPrivPtr->dwCompChar;

	*((LPUNAWORD) ((LPBYTE) lpCompStr + lpCompStr->dwCompReadAttrOffset +
				   dwCursorPos * sizeof(TCHAR))) =
		((ATTR_TARGET_CONVERTED << 8) | ATTR_TARGET_CONVERTED);

	// set reading string lenght for lpCompStr
	if (lpCompStr->dwCompReadStrLen <= dwCursorPos) {
		lpCompStr->dwCompReadStrLen += sizeof(BYTE);
	}
	// composition string is reading string for some simple IMEs
	lpCompStr->dwCompStrLen = lpCompStr->dwCompReadStrLen;

	// composition/reading attribute length is equal to reading string length
	lpCompStr->dwCompReadAttrLen = lpCompStr->dwCompReadStrLen;
	lpCompStr->dwCompAttrLen = lpCompStr->dwCompStrLen;

	// delta start from previous cursor position
	lpCompStr->dwDeltaStart = lpCompStr->dwCursorPos;

	// set new cursor with next to the composition string
	lpCompStr->dwCursorPos = lpCompStr->dwCompStrLen;

	// set lpImcp->iImeState
	imcPrivPtr->iImeState = CST_INPUT;

	// tell app, there is a composition char generated
	imcPrivPtr->fdwImeMsg |= MSG_COMPOSITION;

	imcPrivPtr->fdwGcsFlag |=
		GCS_COMPREAD | GCS_COMP | GCS_CURSORPOS | GCS_DELTASTART;

	return;
}


u32 PASCAL
Finalize(LPINPUTCONTEXT lpIMC,
		 LPCOMPOSITIONSTRING lpCompStr, LPPRIVCONTEXT imcPrivPtr,
		 WORD kbd_char)
{
	LPCANDIDATEINFO lpCandInfo;
	LPCANDIDATELIST lpCandList;
	u32 fEngine;

	if (!lpIMC->hCandInfo) {
		return (0);
	}
	// get lpCandInfo
	lpCandInfo = (LPCANDIDATEINFO) ImmLockIMCC(lpIMC->hCandInfo);

	if (!lpCandInfo) {
		return (0);
	}
	// get lpCandList and init dwCount & dwSelection
	lpCandList = (LPCANDIDATELIST)
		((LPBYTE) lpCandInfo + lpCandInfo->dwOffset[0]);
	lpCandList->dwCount = 0;
	lpCandList->dwSelection = 0;

	// search the IME tables
	fEngine = Engine(lpCompStr, lpCandList, imcPrivPtr, lpIMC, kbd_char);

	if (fEngine == ENGINE_COMP) {
		lpCandInfo->dwCount = 1;

		if (((lpCompStr->dwCursorPos < 3) && (kbd_char != TEXT(' ')))
			|| ((lpCompStr->dwCursorPos == 3)
				&& (kbd_char != TEXT(' ')) && (kbd_char != TEXT('?')))) {

			ImmUnlockIMCC(lpIMC->hCandInfo);
			return (fEngine);
		}

		if (lpCandList->dwCount != 0x0000) {
			// open composition candidate UI window for the string(s)
			if ((imcPrivPtr->
				 fdwImeMsg & (MSG_ALREADY_OPEN | MSG_CLOSE_CANDIDATE)) ==
				(MSG_ALREADY_OPEN | MSG_CLOSE_CANDIDATE)) {
				imcPrivPtr->fdwImeMsg =
					(imcPrivPtr->
					 fdwImeMsg | MSG_CHANGE_CANDIDATE) &
					~(MSG_CLOSE_CANDIDATE);
			} else if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
				imcPrivPtr->fdwImeMsg |= MSG_CHANGE_CANDIDATE;
			} else {
				imcPrivPtr->fdwImeMsg =
					(imcPrivPtr->
					 fdwImeMsg | MSG_OPEN_CANDIDATE) &
					~(MSG_CLOSE_CANDIDATE);
			}

		}

		if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_START) {
			imcPrivPtr->fdwImeMsg |= MSG_COMPOSITION;
		}
	} else if (fEngine == ENGINE_ASCII) {
	} else if (fEngine == ENGINE_RESAULT) {

		// Set lpImep!   and tell application, there is a reslut string
		// imcPrivPtr->fdwImeMsg |= MSG_COMPOSITION;
		// imcPrivPtr->dwCompChar = (DWORD) 0;
		 imcPrivPtr->fdwGcsFlag |= GCS_COMPREAD | GCS_COMP | GCS_CURSORPOS |
		 	GCS_DELTASTART | GCS_RESULTREAD | GCS_RESULT;

		// if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
		// 	imcPrivPtr->fdwImeMsg = (imcPrivPtr->fdwImeMsg | MSG_CLOSE_CANDIDATE) &
		// 		~(MSG_OPEN_CANDIDATE);
		// }
		// // clear  candidate now
		// lpCandList->dwCount = 0;
		// // set iImeState with CST_INIT
		// imcPrivPtr->iImeState = CST_INIT;
		// *(LPDWORD) imcPrivPtr->bSeq = 0;
	}

	ImmUnlockIMCC(lpIMC->hCandInfo);

	return fEngine;
}

/**********************************************************************/
/* CompWord()                                                         */
/**********************************************************************/
void PASCAL CompWord(			// compose the Chinese word(s) according to
						// input key
						WORD kbd_char,
						LPINPUTCONTEXT lpIMC,
						LPCOMPOSITIONSTRING lpCompStr,
						LPPRIVCONTEXT imcPrivPtr, LPGUIDELINE lpGuideLine)
{

	// lpComStr=NULL?
	if (!lpCompStr) {
		MessageBeep((u32) - 1);
		return;
	}
	// escape key
	if (kbd_char == VK_ESCAPE) {	// not good to use VK as char, but...
		CompEscapeKey(lpIMC, lpCompStr, lpGuideLine, imcPrivPtr);
		return;
	}

	// backspace key
	if (kbd_char == TEXT('\b')) {
		CompBackSpaceKey(lpIMC, lpCompStr, imcPrivPtr);
		return;
	}


	if (kbd_char == TEXT(' ')) {
	} else {
		CompStrInfo(lpCompStr, imcPrivPtr, lpGuideLine, kbd_char);
	}

	Finalize(lpIMC, lpCompStr, imcPrivPtr, kbd_char);	// compsition

	return;
}
