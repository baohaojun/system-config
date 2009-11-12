
/*++

Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

Module Name:

    notify.c


++*/


#include <windows.h>
#include <immdev.h>
#include <imedefs.h>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 

void PASCAL
GenerateMessage(HIMC hIMC, LPINPUTCONTEXT lpIMC, LPPRIVCONTEXT imcPrivPtr)
{
	if (!hIMC) {
		return;
	} else if (!lpIMC) {
		return;
	} else if (!imcPrivPtr) {
		return;
	} else if (imcPrivPtr->fdwImeMsg & MSG_IN_IMETOASCIIEX) {
		return;
	} else {
	}

	lpIMC->dwNumMsgBuf += TranslateImeMessage(NULL, lpIMC, imcPrivPtr);

	imcPrivPtr->fdwImeMsg &= (MSG_ALREADY_OPEN | MSG_ALREADY_START);
	imcPrivPtr->fdwGcsFlag = 0;

	ImmGenerateMessage(hIMC);
	return;
}

void PASCAL
GenerateImeMessage(HIMC hIMC, LPINPUTCONTEXT lpIMC, DWORD fdwImeMsg)
{
	LPPRIVCONTEXT imcPrivPtr;

	imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate);
	if (!imcPrivPtr) {
		return;
	}

	imcPrivPtr->fdwImeMsg |= fdwImeMsg;

	if (fdwImeMsg & MSG_CLOSE_CANDIDATE) {
		imcPrivPtr->fdwImeMsg &= ~(MSG_OPEN_CANDIDATE | MSG_CHANGE_CANDIDATE);
	} else if (fdwImeMsg & (MSG_OPEN_CANDIDATE | MSG_CHANGE_CANDIDATE)) {
		imcPrivPtr->fdwImeMsg &= ~(MSG_CLOSE_CANDIDATE);
	}

	if (fdwImeMsg & MSG_END_COMPOSITION) {
		imcPrivPtr->fdwImeMsg &= ~(MSG_START_COMPOSITION);
	} else if (fdwImeMsg & MSG_START_COMPOSITION) {
		imcPrivPtr->fdwImeMsg &= ~(MSG_END_COMPOSITION);
	}

	GenerateMessage(hIMC, lpIMC, imcPrivPtr);

	ImmUnlockIMCC(lpIMC->hPrivate);

	return;
}

void PASCAL CompCancel(HIMC hIMC, LPINPUTCONTEXT lpIMC)
{
	LPPRIVCONTEXT imcPrivPtr;

	if (!lpIMC->hPrivate) {
		return;
	}

	imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate);
	if (!imcPrivPtr) {
		return;
	}

	imcPrivPtr->fdwGcsFlag = (DWORD) 0;

	if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
		CandEscapeKey(lpIMC, imcPrivPtr);
	} else if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_START) {
		LPCOMPOSITIONSTRING lpCompStr;
		LPGUIDELINE lpGuideLine;

		lpCompStr = (LPCOMPOSITIONSTRING) ImmLockIMCC(lpIMC->hCompStr);
		if (!lpCompStr) {
			ImmUnlockIMCC(lpIMC->hCompStr);
			ImmUnlockIMCC(lpIMC->hPrivate);
			return;
		}

		lpGuideLine = (LPGUIDELINE) ImmLockIMCC(lpIMC->hGuideLine);
		if (!lpGuideLine) {
			ImmUnlockIMCC(lpIMC->hGuideLine);
			ImmUnlockIMCC(lpIMC->hPrivate);
			return;
		}

		CompEscapeKey(lpIMC, lpCompStr, lpGuideLine, imcPrivPtr);

		if (lpGuideLine) {
			ImmUnlockIMCC(lpIMC->hGuideLine);
		}
		if (lpCompStr) {
			ImmUnlockIMCC(lpIMC->hCompStr);
		}
	} else {
		ImmUnlockIMCC(lpIMC->hPrivate);
		return;
	}

	GenerateMessage(hIMC, lpIMC, imcPrivPtr);

	ImmUnlockIMCC(lpIMC->hPrivate);

	return;
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

void PASCAL NotifySelectCand(	// app tell IME that one candidate string is
								// selected (by mouse or non keyboard action
								// - for example sound)
								HIMC hIMC,
								LPINPUTCONTEXT lpIMC,
								LPCANDIDATEINFO lpCandInfo, DWORD dwIndex,
								DWORD dwValue)
{
	LPCANDIDATELIST lpCandList;
	LPCOMPOSITIONSTRING lpCompStr;
	LPPRIVCONTEXT imcPrivPtr;

	if (!lpCandInfo) {
		return;
	}

	if (dwIndex >= lpCandInfo->dwCount) {
		// wanted candidate list is not created yet!
		return;
	} else if (dwIndex == 0) {
		if (lpIMC->fdwConversion & 0) {
			return;				// not implemented yet
		}
	}

	lpCandList = (LPCANDIDATELIST)
		((LPBYTE) lpCandInfo + lpCandInfo->dwOffset[0]);

	// the selected value even more than the number of total candidate
	// strings, it is imposible. should be error of app
	if (dwValue >= lpCandList->dwCount) {
		return;
	}
	// app select this candidate string
	lpCandList->dwSelection = dwValue;

	lpCompStr = (LPCOMPOSITIONSTRING) ImmLockIMCC(lpIMC->hCompStr);
	if (!lpCompStr) {
		return;
	}
	imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate);
	if (!lpCompStr) {
		ImmUnlockIMCC(lpIMC->hCompStr);
		return;
	}
	// translate into message buffer
	SelectOneCand(lpIMC, lpCompStr, imcPrivPtr, lpCandList);

	// let app generate those messages in its message loop
	GenerateMessage(hIMC, lpIMC, imcPrivPtr);

	ImmUnlockIMCC(lpIMC->hPrivate);
	ImmUnlockIMCC(lpIMC->hCompStr);

	return;
}

BOOL WINAPI
NotifyIME(HIMC hIMC, DWORD dwAction, DWORD dwIndex, DWORD dwValue)
{
	return FALSE;
}
