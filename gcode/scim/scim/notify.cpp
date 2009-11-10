
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
	EnterLeaveDebug(); 

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
	EnterLeaveDebug(); 
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


//we don't allow SetCompPosition
//we don't know what it does!

BOOL WINAPI
ImeSetCompositionString(HIMC hIMC,
						DWORD dwIndex,
						LPVOID lpComp,
						DWORD dwCompLen, LPVOID lpRead, DWORD dwReadLen)
{
	EnterLeaveDebug(); 
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
		if (lpIMC->fdwConversion & IME_CMODE_CHARCODE) {
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

/**********************************************************************/
/* NotifyIME()                                                        */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL WINAPI
NotifyIME(HIMC hIMC, DWORD dwAction, DWORD dwIndex, DWORD dwValue)
{
	LPINPUTCONTEXT lpIMC;
	DWORD fdwImeMsg;
	BOOL fRet;

	fRet = FALSE;

	if (!hIMC) {
		return (fRet);
	}

	switch (dwAction) {
	case NI_OPENCANDIDATE:		// after a composition string is determined
		// if an IME can open candidate, it will.
		// if it can not, app also can not open it.
	case NI_CLOSECANDIDATE:
		return (fRet);			// not supported
	case NI_SELECTCANDIDATESTR:
	case NI_SETCANDIDATE_PAGESTART:
	case NI_SETCANDIDATE_PAGESIZE:
		break;					// need to handle it
	case NI_CHANGECANDIDATELIST:
		break;
	case NI_CONTEXTUPDATED:
		switch (dwValue) {
		case IMC_SETCONVERSIONMODE:
		case IMC_SETOPENSTATUS:
			break;				// need to handle it
		case IMC_SETCANDIDATEPOS:
		case IMC_SETCOMPOSITIONFONT:
		case IMC_SETCOMPOSITIONWINDOW:
			return (TRUE);		// not important to the IME
		default:
			return (fRet);		// not supported
		}
		break;
	case NI_COMPOSITIONSTR:
		switch (dwIndex) {
		case CPS_COMPLETE:
			break;				// need to handle it
		case CPS_CONVERT:		// all composition string can not be convert
		case CPS_REVERT:		// any more, it maybe work for some
			// intelligent phonetic IMEs
			return (fRet);
		case CPS_CANCEL:
			break;				// need to handle it
		default:
			return (fRet);		// not supported
		}
		break;					// need to handle it
	default:
		return (fRet);			// not supported
	}

	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
	if (!lpIMC) {
		return (fRet);
	}

	fRet = TRUE;

	switch (dwAction) {
	case NI_CONTEXTUPDATED:
		switch (dwValue) {
		case IMC_SETCONVERSIONMODE:
			if ((lpIMC->fdwConversion ^ dwIndex) & IME_CMODE_CHARCODE) {
				// reject CHARCODE
				lpIMC->fdwConversion &= ~IME_CMODE_CHARCODE;
				MessageBeep((UINT) - 1);
				break;
			}

			fdwImeMsg = 0;

			if ((lpIMC->fdwConversion ^ dwIndex) & IME_CMODE_NOCONVERSION) {
				lpIMC->fdwConversion |= IME_CMODE_NATIVE;
				lpIMC->fdwConversion &= ~(IME_CMODE_CHARCODE |
										  IME_CMODE_EUDC |
										  IME_CMODE_SYMBOL);
			}

			if ((lpIMC->fdwConversion ^ dwIndex) & IME_CMODE_EUDC) {
				lpIMC->fdwConversion |= IME_CMODE_NATIVE;
				lpIMC->fdwConversion &= ~(IME_CMODE_CHARCODE |
										  IME_CMODE_NOCONVERSION |
										  IME_CMODE_SYMBOL);
			}

			if ((lpIMC->fdwConversion ^ dwIndex) == IME_CMODE_NATIVE) {
				lpIMC->fdwConversion &= ~(IME_CMODE_CHARCODE |
										  IME_CMODE_NOCONVERSION |
										  IME_CMODE_EUDC);
			}

			if (fdwImeMsg) {
				GenerateImeMessage(hIMC, lpIMC, fdwImeMsg);
			}

			CompCancel(hIMC, lpIMC);
			break;
		case IMC_SETOPENSTATUS:
			CompCancel(hIMC, lpIMC);
			break;
		default:
			break;
		}
		break;
	case NI_SELECTCANDIDATESTR:
		if (!lpIMC->fOpen) {
			fRet = FALSE;
			break;
		} else if (lpIMC->fdwConversion & IME_CMODE_NOCONVERSION) {
			fRet = FALSE;
			break;
		} else if (lpIMC->fdwConversion & IME_CMODE_EUDC) {
			fRet = FALSE;
			break;
		} else if (!lpIMC->hCandInfo) {
			fRet = FALSE;
			break;
		} else {
			LPCANDIDATEINFO lpCandInfo;

			lpCandInfo = (LPCANDIDATEINFO) ImmLockIMCC(lpIMC->hCandInfo);
			if (!lpCandInfo) {
				fRet = FALSE;
				break;
			}

			NotifySelectCand(hIMC, lpIMC, lpCandInfo, dwIndex, dwValue);

			ImmUnlockIMCC(lpIMC->hCandInfo);
		}

		break;
	case NI_CHANGECANDIDATELIST:
		fdwImeMsg = 0;

		fdwImeMsg |= MSG_CHANGE_CANDIDATE;
		GenerateImeMessage(hIMC, lpIMC, fdwImeMsg);

		break;
	case NI_SETCANDIDATE_PAGESTART:
	case NI_SETCANDIDATE_PAGESIZE:
		if (dwIndex != 0) {
			fRet = FALSE;
			break;
		} else if (!lpIMC->fOpen) {
			fRet = FALSE;
			break;
		} else if (lpIMC->fdwConversion & IME_CMODE_NOCONVERSION) {
			fRet = FALSE;
			break;
		} else if (lpIMC->
				   fdwConversion & (IME_CMODE_EUDC | IME_CMODE_SYMBOL)) {
			fRet = FALSE;
			break;
		} else if (!lpIMC->hCandInfo) {
			fRet = FALSE;
			break;
		} else {
			LPCANDIDATEINFO lpCandInfo;
			LPCANDIDATELIST lpCandList;

			lpCandInfo = (LPCANDIDATEINFO) ImmLockIMCC(lpIMC->hCandInfo);
			if (!lpCandInfo) {
				fRet = FALSE;
				break;
			}

			lpCandList = (LPCANDIDATELIST) ((LPBYTE) lpCandInfo +
											lpCandInfo->dwOffset[0]);

			if (dwAction == NI_SETCANDIDATE_PAGESTART) {
				if (dwValue < lpCandList->dwCount) {
					lpCandList->dwPageStart = lpCandList->dwSelection =
						dwValue;
				}
			} else {
				if (lpCandList->dwCount) {
					lpCandList->dwPageSize = dwValue;
				}
			}

			ImmUnlockIMCC(lpIMC->hCandInfo);
		}

		break;
	case NI_COMPOSITIONSTR:
		switch (dwIndex) {
		case CPS_CANCEL:
			CompCancel(hIMC, lpIMC);
			break;
		case CPS_COMPLETE:
			{
				LPPRIVCONTEXT imcPrivPtr;

				imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate);
				if (!imcPrivPtr) {
					break;
				}

				if (imcPrivPtr->iImeState == CST_INIT) {
					CompCancel(hIMC, lpIMC);
					// can not do any thing
				} else if (imcPrivPtr->iImeState == CST_CHOOSE) {
					LPCOMPOSITIONSTRING lpCompStr;
					LPCANDIDATEINFO lpCandInfo;

					lpCompStr =
						(LPCOMPOSITIONSTRING) ImmLockIMCC(lpIMC->hCompStr);
					if (!lpCompStr) {
						break;
					}


					lpCandInfo =
						(LPCANDIDATEINFO) ImmLockIMCC(lpIMC->hCandInfo);
					if (lpCandInfo) {
						LPCANDIDATELIST lpCandList;

						lpCandList =
							(LPCANDIDATELIST) ((LPBYTE) lpCandInfo +
											   lpCandInfo->dwOffset[0]);

						SelectOneCand(lpIMC, lpCompStr, imcPrivPtr,
									  lpCandList);

						ImmUnlockIMCC(lpIMC->hCandInfo);

						GenerateMessage(hIMC, lpIMC, imcPrivPtr);
					}

					if (lpCompStr)
						ImmUnlockIMCC(lpIMC->hCompStr);
				} else if ((lpIMC->fdwConversion & (IME_CMODE_NATIVE |
													IME_CMODE_EUDC |
													IME_CMODE_SYMBOL)) !=
						   IME_CMODE_NATIVE) {
					CompCancel(hIMC, lpIMC);
				} else if (imcPrivPtr->iImeState == CST_INPUT) {
					LPCOMPOSITIONSTRING lpCompStr;
					LPGUIDELINE lpGuideLine;
					LPCANDIDATEINFO lpCandInfo;

					lpCompStr =
						(LPCOMPOSITIONSTRING) ImmLockIMCC(lpIMC->hCompStr);
					if (!lpCompStr) {
						break;
					}

					lpGuideLine =
						(LPGUIDELINE) ImmLockIMCC(lpIMC->hGuideLine);
					if (!lpGuideLine) {
						ImmUnlockIMCC(lpIMC->hCompStr);
						break;
					}


					CompWord(' ', lpIMC, lpCompStr, imcPrivPtr, lpGuideLine);

					if (imcPrivPtr->iImeState == CST_INPUT) {
						CompCancel(hIMC, lpIMC);
					} else if (imcPrivPtr->iImeState != CST_CHOOSE) {
					} else if (lpCandInfo =
							   (LPCANDIDATEINFO) ImmLockIMCC(lpIMC->
															 hCandInfo)) {
						LPCANDIDATELIST lpCandList;

						lpCandList =
							(LPCANDIDATELIST) ((LPBYTE) lpCandInfo +
											   lpCandInfo->dwOffset[0]);

//                        SelectOneCand(hIMC, lpIMC, lpCompStr, imcPrivPtr, lpCandList);
						SelectOneCand(lpIMC, lpCompStr, imcPrivPtr,
									  lpCandList);

						ImmUnlockIMCC(lpIMC->hCandInfo);
					} else {
					}

					if (lpCompStr)
						ImmUnlockIMCC(lpIMC->hCompStr);
					if (lpGuideLine)
						ImmUnlockIMCC(lpIMC->hGuideLine);

					// don't phrase predition under this case
					if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
						imcPrivPtr->fdwImeMsg =
							(imcPrivPtr->
							 fdwImeMsg | MSG_CLOSE_CANDIDATE) &
							~(MSG_OPEN_CANDIDATE | MSG_CHANGE_CANDIDATE);
					} else {
						imcPrivPtr->fdwImeMsg &=
							~(MSG_CLOSE_CANDIDATE | MSG_OPEN_CANDIDATE);
					}

					GenerateMessage(hIMC, lpIMC, imcPrivPtr);
				} else {
					CompCancel(hIMC, lpIMC);
				}

				ImmUnlockIMCC(lpIMC->hPrivate);
			}
			break;
		default:
			break;
		}
		break;
	default:
		break;
	}

	ImmUnlockIMC(hIMC);
	return (fRet);
}
