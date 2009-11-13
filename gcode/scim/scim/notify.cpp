
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
#include "imewnd.h"

void PASCAL
GenerateMessage(HIMC hIMC, input_context& ic, LPPRIVCONTEXT imcPrivPtr)
{
	if (!ic) {
		return;
	} else if (!imcPrivPtr) {
		return;
	} else if (imcPrivPtr->fdwImeMsg & MSG_IN_IMETOASCIIEX) {
		return;
	} else {
	}

	ic->dwNumMsgBuf += 0;

	imcPrivPtr->fdwImeMsg &= (MSG_ALREADY_OPEN | MSG_ALREADY_START);
	imcPrivPtr->fdwGcsFlag = 0;

	ImmGenerateMessage(hIMC);
	return;
}

void PASCAL
GenerateImeMessage(HIMC hIMC, input_context& ic, DWORD fdwImeMsg)
{
	LPPRIVCONTEXT imcPrivPtr;

	imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(ic->hPrivate);
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

	GenerateMessage(hIMC, ic, imcPrivPtr);

	ImmUnlockIMCC(ic->hPrivate);

	return;
}

void PASCAL CompCancel(HIMC hIMC, input_context& ic)
{
	LPPRIVCONTEXT imcPrivPtr;

	if (!ic->hPrivate) {
		return;
	}

	imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(ic->hPrivate);
	if (!imcPrivPtr) {
		return;
	}

	imcPrivPtr->fdwGcsFlag = (DWORD) 0;

	if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
		CandEscapeKey(ic, imcPrivPtr);
	} else if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_START) {
		LPCOMPOSITIONSTRING lpCompStr;
		LPGUIDELINE lpGuideLine;

		lpCompStr = (LPCOMPOSITIONSTRING) ImmLockIMCC(ic->hCompStr);
		if (!lpCompStr) {
			ImmUnlockIMCC(ic->hCompStr);
			ImmUnlockIMCC(ic->hPrivate);
			return;
		}

		lpGuideLine = (LPGUIDELINE) ImmLockIMCC(ic->hGuideLine);
		if (!lpGuideLine) {
			ImmUnlockIMCC(ic->hGuideLine);
			ImmUnlockIMCC(ic->hPrivate);
			return;
		}

		CompEscapeKey(ic, lpCompStr, lpGuideLine, imcPrivPtr);

		if (lpGuideLine) {
			ImmUnlockIMCC(ic->hGuideLine);
		}
		if (lpCompStr) {
			ImmUnlockIMCC(ic->hCompStr);
		}
	} else {
		ImmUnlockIMCC(ic->hPrivate);
		return;
	}

	GenerateMessage(hIMC, ic, imcPrivPtr);

	ImmUnlockIMCC(ic->hPrivate);

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

BOOL WINAPI
NotifyIME(HIMC hIMC, DWORD dwAction, DWORD dwIndex, DWORD dwValue)
{
	return FALSE;
}
