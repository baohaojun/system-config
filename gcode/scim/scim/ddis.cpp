
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
HWND hCrtDlg = NULL;

BOOL WINAPI
ImeInquire(LPIMEINFO lpImeInfo, LPTSTR lpszWndCls, DWORD dwSystemInfoFlags)
{
	if (!lpImeInfo) {
		return (FALSE);
	}

	lpImeInfo->dwPrivateDataSize = sizeof(PRIVCONTEXT);
	lpImeInfo->fdwProperty = IME_PROP_KBD_CHAR_FIRST |
		IME_PROP_UNICODE |
		IME_PROP_CANDLIST_START_FROM_1 | IME_PROP_IGNORE_UPKEYS;

	lpImeInfo->fdwConversionCaps =
		IME_CMODE_NATIVE | IME_CMODE_CHARCODE | IME_CMODE_NOCONVERSION;
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

/**********************************************************************/
/* ImeSetDlgProc()                                                 */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL FAR PASCAL ImeSetDlgProc(	// dialog procedure of configuration
								 HWND hDlg, UINT uMessage, WORD wParam,
								 LONG lParam)
{
	RECT rc;
	LONG DlgWidth, DlgHeight;
	static DWORD TempParam;


	switch (uMessage) {
	case WM_INITDIALOG:
		hCrtDlg = hDlg;
		// reset position
		GetWindowRect(hDlg, &rc);
		DlgWidth = rc.right - rc.left;
		DlgHeight = rc.bottom - rc.top;


		SetWindowPos(hDlg, HWND_TOP,
					 (int) (sImeG.rcWorkArea.right - DlgWidth) / 2,
					 (int) (sImeG.rcWorkArea.bottom - DlgHeight) / 2,
					 (int) 0, (int) 0, SWP_NOSIZE);


		return (TRUE);			// don't want to set focus to special control
	case WM_COMMAND:
		switch (wParam) {
		case IDOK:
			{
				HKEY hKeyCurrVersion;
				HKEY hKeyGB;
				DWORD retCode;
				//CHAR  Buf[LINE_LEN];


				retCode = OpenReg_PathSetup(&hKeyCurrVersion);

				if (retCode) {
					RegCreateKey(HKEY_CURRENT_USER,
								 REGSTR_PATH_SETUP, &hKeyCurrVersion);
				}


				if (hKeyCurrVersion != NULL) {
					retCode = RegCreateKeyEx(hKeyCurrVersion,
											 szImeRegName,
											 0,
											 NULL,
											 REG_OPTION_NON_VOLATILE,
											 KEY_ALL_ACCESS,
											 NULL, &hKeyGB, NULL);
				}

				if (hKeyGB != NULL) {

					RegCloseKey(hKeyGB);
				}

				if (hKeyCurrVersion)
					RegCloseKey(hKeyCurrVersion);

			}
			EndDialog(hDlg, FALSE);
			break;
		case IDCANCEL:
			EndDialog(hDlg, FALSE);
			break;
		default:
			return (FALSE);
		}
		return (TRUE);
	case WM_PAINT:
		{
			RECT rc;

			GetClientRect(hDlg, &rc);
			DrawConvexRect(GetDC(hDlg),
						   rc.left + 7,
						   rc.top + 7, rc.right - 7 - 1,
						   rc.bottom - 40 - 1);

			DrawConvexRectP(GetDC(hDlg),
							rc.left + 7,
							rc.top + 7, rc.right - 7, rc.bottom - 40);
		}

		return (FALSE);
	case WM_CLOSE:
		EndDialog(hDlg, FALSE);
		return (TRUE);
	default:
		return (FALSE);
	}

	return (TRUE);
}

/**********************************************************************/
/* ImeConfigure()                                                     */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
/*BOOL WINAPI ImeConfigure(      // configurate the IME setting
    HKL     hKL,               // hKL of this IME
    HWND    hAppWnd,           // the owner window
    DWORD   dwMode)            // mode of dialog
{*/
BOOL WINAPI ImeConfigure(		// configurate the IME setting
							HKL hKL,	// hKL of this IME
							HWND hAppWnd,	// the owner window
							DWORD dwMode, LPVOID lpData)	// mode of dialog
{
	switch (dwMode) {
	case IME_CONFIG_GENERAL:
		DialogBox(hInst, TEXT("ImeSet"), (HWND) hAppWnd,
				  (DLGPROC) ImeSetDlgProc);
		break;
	default:
		return (FALSE);
		break;
	}
	return (TRUE);
}


/**********************************************************************/
/* XGBConversion()                                                       */
/**********************************************************************/
DWORD PASCAL
XGBConversion(LPCTSTR lpszReading, LPCANDIDATELIST lpCandList,
			  UINT uBufLen)
{
	UINT MAX_COMP;
	UINT uMaxCand;
	UINT iRet;
	WORD wCode;
	LPPRIVCONTEXT lpImcP;
	HGLOBAL hImcP;
	int i;
	DWORD dwSize;
	if (!(lstrlen(lpszReading) == 4)) {
		return (0);
	}

	hImcP = GlobalAlloc(GMEM_MOVEABLE, sizeof(PRIVCONTEXT));
	if (!hImcP) {
		return (0);
	}
	lpImcP = (LPPRIVCONTEXT) GlobalLock(hImcP);
	if (!lpImcP) {
		GlobalFree(hImcP);
		return (0);
	}
	lstrcpy(lpImcP->bSeq, lpszReading);
	if (lpImcP->bSeq[3] == TEXT('?')) {
		MAX_COMP = 178;
	} else {
		MAX_COMP = 1;
	}
	dwSize =					// similar to ClearCand
		// header length
		sizeof(CANDIDATELIST) +
		// candidate string pointers
		sizeof(DWORD) * MAX_COMP +
		// string plus NULL terminator
		(sizeof(WORD) + sizeof(TCHAR)) * MAX_COMP;
	if (!uBufLen) {
		return (dwSize);
	}

	uMaxCand = uBufLen - sizeof(CANDIDATELIST);

	uMaxCand /= sizeof(DWORD) + sizeof(WORD) + sizeof(TCHAR);
	if (!uMaxCand) {
		// can not even put one string
		return (0);
	}

	lpCandList->dwSize = dwSize;
	lpCandList->dwStyle = IME_CAND_READ;	// candidate having same reading
	lpCandList->dwCount = 0;
	lpCandList->dwSelection = 0;
	lpCandList->dwPageSize = CANDPERPAGE;
	lpCandList->dwOffset[0] = sizeof(CANDIDATELIST) + sizeof(DWORD) *
		(uMaxCand - 1);
	lpImcP->bSeq[0] = 0;
	lpImcP->bSeq[1] = 0;
	lpImcP->bSeq[2] = 0;
	lpImcP->bSeq[3] = 0;

	for (i = 0; i < 4; i++) {
		iRet = XGBProcessKey(*(LPBYTE) ((LPBYTE) lpszReading + i), lpImcP);
		if (iRet == CST_INPUT) {
			lpImcP->bSeq[i] = *(LPBYTE) ((LPBYTE) lpszReading + i);
		} else {
			return (DWORD) 0;
		}
	}
	wCode = XGBEngine(lpImcP);

	wCode = HIBYTE(wCode) | (LOBYTE(wCode) << 8);

	for (i = 0; i < (0x7e - 0x40 + 1); i++, wCode++) {
		XGBAddCodeIntoCand(lpCandList, wCode);
	}
	wCode++;
	for (i = 0; i < (0xfe - 0x80 + 1); i++, wCode++) {
		XGBAddCodeIntoCand(lpCandList, wCode);
	}

	GlobalUnlock(hImcP);
	GlobalFree(hImcP);
	return (dwSize);
}

/**********************************************************************/
/* Conversion()                                                       */
/**********************************************************************/
DWORD PASCAL
Conversion(LPCTSTR lpszReading, LPCANDIDATELIST lpCandList, UINT uBufLen)
{
	UINT MAX_COMP, i;
	UINT uMaxCand;
	UINT iRet;
	WORD wCode;
	LPPRIVCONTEXT lpImcP;
	HGLOBAL hImcP;

	DWORD dwSize;
	if (!(lstrlen(lpszReading) == 4)) {
		return (0);
	}

	hImcP = GlobalAlloc(GMEM_MOVEABLE, sizeof(PRIVCONTEXT));
	if (!hImcP) {
		return (0);
	}
	lpImcP = (LPPRIVCONTEXT) GlobalLock(hImcP);
	if (!lpImcP) {
		GlobalFree(hImcP);
		return (0);
	}
	lstrcpy(lpImcP->bSeq, lpszReading);
	if (lpImcP->bSeq[3] == TEXT('?')) {
		MAX_COMP = 94;
	} else {
		MAX_COMP = 1;
	}
	dwSize =					// similar to ClearCand
		// header length
		sizeof(CANDIDATELIST) +
		// candidate string pointers
		sizeof(DWORD) * MAX_COMP +
		// string plus NULL terminator
		(sizeof(WORD) + sizeof(TCHAR)) * MAX_COMP;
	if (!uBufLen) {
		return (dwSize);
	}

	uMaxCand = uBufLen - sizeof(CANDIDATELIST);

	uMaxCand /= sizeof(DWORD) + sizeof(WORD) + sizeof(TCHAR);
	if (!uMaxCand) {
		// can not even put one string
		return (0);
	}

	lpCandList->dwSize = dwSize;
	lpCandList->dwStyle = IME_CAND_READ;	// candidate having same reading
	lpCandList->dwCount = 0;
	lpCandList->dwSelection = 0;
	lpCandList->dwPageSize = CANDPERPAGE;
	lpCandList->dwOffset[0] = sizeof(CANDIDATELIST) + sizeof(DWORD) *
		(uMaxCand - 1);
	lpImcP->bSeq[0] = 0;
	lpImcP->bSeq[1] = 0;
	lpImcP->bSeq[2] = 0;
	lpImcP->bSeq[3] = 0;

	for (i = 0; i < 4; i++) {
		iRet = GBProcessKey(*(LPBYTE) ((LPBYTE) lpszReading + i), lpImcP);
		if (iRet == CST_INPUT) {
			lpImcP->bSeq[i] = *(LPBYTE) ((LPBYTE) lpszReading + i);
		} else {
			return (DWORD) 0;
		}
	}
	wCode = GBEngine(lpImcP);
	wCode = HIBYTE(wCode) | (LOBYTE(wCode) << 8);
	for (i = 0; i < MAX_COMP; i++, wCode++) {
		AddCodeIntoCand(lpCandList, wCode);
	}

	GlobalUnlock(hImcP);
	GlobalFree(hImcP);
	return (dwSize);
}

/**************************************************************************
BOOL DBCSToGBCode ( WORD    wCode, BYTE    AbSeq[5])
***************************************************************************/
BOOL DBCSToGBCode(WORD wCode, TCHAR AbSeq[5])
{
	WORD AreaCode;

	//Converte Unicode to GBK
	// change CP_ACP to 936, so that it can work under Multilingul Env.
	WideCharToMultiByte(NATIVE_ANSI_CP, WC_COMPOSITECHECK,
						(LPCWSTR) & wCode, 1, (char *) &AreaCode, 2, NULL,
						NULL);
	wCode = AreaCode;

//check valid GB range code first
	WORD tmp;
	tmp = HIBYTE(wCode) | (LOBYTE(wCode) << 8);
	wsprintf(AbSeq, TEXT("%04x"), tmp);

	return TRUE;
}

/***************************************************************************
BOOL AreaToGB ( BYTE    AbSeq[5],BYTE    GbSeq[5])
***************************************************************************/
BOOL AreaToGB(TCHAR AbSeq[5], TCHAR GbSeq[5])
{
	TCHAR MbSeq[3];				// Temp string
	// Area turn
	wsprintf(MbSeq, TEXT("%lx"), (AbSeq[0] * 10 + AbSeq[1] + 0xa0));
	GbSeq[0] = MbSeq[0];
	GbSeq[1] = MbSeq[1];
	//position turn
	wsprintf(MbSeq, TEXT("%lx"), (AbSeq[2] * 10 + AbSeq[3] + 0xa0));
	GbSeq[2] = MbSeq[0];
	GbSeq[3] = MbSeq[1];
	GbSeq[4] = TEXT('\0');
	return TRUE;
}

/**********************************************************************/
/* UnicodeReverseConversion()                                                */
/**********************************************************************/
DWORD PASCAL
UnicodeReverseConversion(WORD wCode,
						 LPCANDIDATELIST lpCandList, UINT uBufLen)
{
	UINT MAX_COMP = 1;
	UINT nMaxKey = 4;
	TCHAR AbSeq[5];
	UINT uMaxCand;
	DWORD dwSize =				// similar to ClearCand
		// header length
		sizeof(CANDIDATELIST) +
		// candidate string pointers
		sizeof(DWORD) * MAX_COMP +
		// string plus NULL terminator
		(sizeof(TCHAR) * nMaxKey + sizeof(TCHAR));

	if (!uBufLen) {
		return (dwSize);
	}

	uMaxCand = uBufLen - sizeof(CANDIDATELIST);

	uMaxCand /= sizeof(DWORD) + (sizeof(TCHAR) * nMaxKey + sizeof(TCHAR));
	if (uMaxCand == 0) {
		// can not put one string
		return (0);
	}

	lpCandList->dwSize = sizeof(CANDIDATELIST) +
		sizeof(DWORD) * uMaxCand + (sizeof(TCHAR) * nMaxKey +
									sizeof(TCHAR));
	lpCandList->dwStyle = IME_CAND_READ;
	lpCandList->dwCount = 0;
	lpCandList->dwSelection = 0;
	//lpCandList->dwPageSize = CANDPERPAGE; New Spac
	lpCandList->dwOffset[0] = sizeof(CANDIDATELIST) + sizeof(DWORD);


	wsprintf(AbSeq, TEXT("%04x"), wCode);
	lstrcpy((LPTSTR) ((LPBYTE) lpCandList + lpCandList->dwOffset[0]),
			AbSeq);


	// string count ++
	lpCandList->dwCount = 1;

	return (dwSize);
}

/**********************************************************************/
/* XGBReverseConversion()                                                */
/**********************************************************************/
DWORD PASCAL
XGBReverseConversion(WORD wCode, LPCANDIDATELIST lpCandList, UINT uBufLen)
{
	UINT MAX_COMP = 1;
	UINT nMaxKey = 4;
	TCHAR AbSeq[5];
	UINT uMaxCand;
	DWORD dwSize =				// similar to ClearCand
		// header length
		sizeof(CANDIDATELIST) +
		// candidate string pointers
		sizeof(DWORD) * MAX_COMP +
		// string plus NULL terminator
		(sizeof(TCHAR) * nMaxKey + sizeof(TCHAR));

	if (!uBufLen) {
		return (dwSize);
	}

	uMaxCand = uBufLen - sizeof(CANDIDATELIST);

	uMaxCand /= sizeof(DWORD) + (sizeof(TCHAR) * nMaxKey + sizeof(TCHAR));
	if (uMaxCand == 0) {
		// can not put one string
		return (0);
	}

	lpCandList->dwSize = sizeof(CANDIDATELIST) +
		sizeof(DWORD) * uMaxCand + (sizeof(TCHAR) * nMaxKey +
									sizeof(TCHAR));
	lpCandList->dwStyle = IME_CAND_READ;
	lpCandList->dwCount = 0;
	lpCandList->dwSelection = 0;
	//lpCandList->dwPageSize = CANDPERPAGE; New Spac
	lpCandList->dwOffset[0] = sizeof(CANDIDATELIST) + sizeof(DWORD);

	if (!DBCSToGBCode(wCode, AbSeq))
		return 0;				//actual is DBCSToGBInternalCode


	lstrcpy((LPTSTR) ((LPBYTE) lpCandList + lpCandList->dwOffset[0]),
			AbSeq);


	// string count ++
	lpCandList->dwCount = 1;

	return (dwSize);
}


/**********************************************************************/
/* ReverseConversion()                                                */
/**********************************************************************/
DWORD PASCAL
ReverseConversion(WORD wCode, LPCANDIDATELIST lpCandList, UINT uBufLen)
{
	UINT MAX_COMP = 2;
	UINT nMaxKey = 4;
	TCHAR AbSeq[5];
	TCHAR GbSeq[5];
	UINT uMaxCand;
	DWORD dwSize =				// similar to ClearCand
		// header length
		sizeof(CANDIDATELIST) +
		// candidate string pointers
		sizeof(DWORD) * MAX_COMP +
		// string plus NULL terminator
		(sizeof(TCHAR) * nMaxKey + sizeof(TCHAR));

	if (!uBufLen) {
		return (dwSize);
	}

	uMaxCand = uBufLen - sizeof(CANDIDATELIST);

	uMaxCand /= sizeof(DWORD) + (sizeof(TCHAR) * nMaxKey + sizeof(TCHAR));
	if (uMaxCand == 0) {
		// can not put one string
		return (0);
	}

	lpCandList->dwSize = sizeof(CANDIDATELIST) +
		sizeof(DWORD) * uMaxCand + (sizeof(TCHAR) * nMaxKey +
									sizeof(TCHAR));
	lpCandList->dwStyle = IME_CAND_READ;
	lpCandList->dwCount = 0;
	lpCandList->dwSelection = 0;
	//lpCandList->dwPageSize = CANDPERPAGE; New Spac
	lpCandList->dwOffset[0] = sizeof(CANDIDATELIST) + sizeof(DWORD);

	if (!DBCSToGBCode(wCode, AbSeq))
		return 0;
	AreaToGB(AbSeq, GbSeq);
	AbSeq[1] += TEXT('0');
	AbSeq[0] += TEXT('0');
	AbSeq[3] += TEXT('0');
	AbSeq[2] += TEXT('0');

	lstrcpy((LPTSTR) ((LPBYTE) lpCandList + lpCandList->dwOffset[0]),
			AbSeq);
	lpCandList->dwOffset[1] =
		lpCandList->dwOffset[0] + 4 * sizeof(TCHAR) + sizeof(TCHAR);

	lstrcpy((LPTSTR) ((LPBYTE) lpCandList + lpCandList->dwOffset[1]),
			GbSeq);


	// string count ++
	lpCandList->dwCount = 2;

	return (dwSize);
}

/**********************************************************************/
/* ImeConversionList()                                                */
/**********************************************************************/
DWORD WINAPI
ImeConversionList(HIMC hIMC,
				  LPCTSTR lpszSrc,
				  LPCANDIDATELIST lpCandList, DWORD uBufLen, UINT uFlag)
{
	WORD wCode;
	LPINPUTCONTEXT lpIMC;
	LPPRIVCONTEXT lpImcP;

	if (!uBufLen) {
	} else if (!lpszSrc) {
		return (0);
	} else if (!*lpszSrc) {
		return (0);
	} else if (!lpCandList) {
		return (0);
	} else if (uBufLen <= sizeof(CANDIDATELIST)) {
		// buffer size can not even put the header information
		return (0);
	}

	switch (uFlag) {
	case GCL_CONVERSION:
		lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
		if (!lpIMC) {
			return (FALSE);
		}
		lpImcP = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate);
		if (!lpImcP) {
			ImmUnlockIMC(hIMC);
			return (FALSE);
		}
		return (XGBConversion(lpszSrc, lpCandList, uBufLen));
		break;
	case GCL_REVERSECONVERSION:
		if (!uBufLen) {
			return 1;
		}
		// only support one DBCS char reverse conversion
		if (*(LPTSTR) ((LPBYTE) lpszSrc + sizeof(WORD)) != TEXT('\0')) {
			return (0);
		}

		wCode = *(LPWORD) lpszSrc;

		// swap lead byte & second byte, UNICODE don't need it
		// wCode = HIBYTE(wCode) | (LOBYTE(wCode) << 8);  For Big5

		return (UnicodeReverseConversion(wCode, lpCandList, uBufLen));

		break;
	default:
		return (0);
		break;
	}

	return (0);
}

/**********************************************************************/
/* ImeDestroy()                                                       */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL WINAPI ImeDestroy(			// this dll is unloaded
						  UINT uReserved)
{
	if (uReserved) {
		return (FALSE);
	}

	return (TRUE);
}

/**********************************************************************/
/* ImeEscape()                                                        */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
#define IME_INPUTKEYTOSEQUENCE  0x22

LRESULT WINAPI ImeEscape(		// escape function of IMEs
							HIMC hIMC, UINT uSubFunc, LPVOID lpData)
{
	LRESULT lRet;

	switch (uSubFunc) {
	case IME_ESC_QUERY_SUPPORT:

		if (lpData == NULL)
			return FALSE;

		switch (*(LPUINT) lpData) {
		case IME_ESC_QUERY_SUPPORT:
		case IME_ESC_MAX_KEY:
		case IME_ESC_IME_NAME:
		case IME_ESC_GETHELPFILENAME:
			return (TRUE);
		case IME_ESC_SEQUENCE_TO_INTERNAL:
		case IME_ESC_GET_EUDC_DICTIONARY:
		case IME_ESC_SET_EUDC_DICTIONARY:
		case IME_INPUTKEYTOSEQUENCE:	// will not supported in next version
			return (FALSE);		// will not supported in GB IME
		default:
			return (FALSE);

		}
		break;
	case IME_ESC_SEQUENCE_TO_INTERNAL:
	case IME_ESC_GET_EUDC_DICTIONARY:
	case IME_ESC_SET_EUDC_DICTIONARY:
	case IME_INPUTKEYTOSEQUENCE:
		return (FALSE);
	case IME_ESC_MAX_KEY:
		return ((WORD) 4);
	case IME_ESC_IME_NAME:

		if (lpData == NULL)
			return FALSE;

		lstrcpy((LPWSTR) lpData, szImeName);
		return (TRUE);

	case IME_ESC_GETHELPFILENAME:
		{
			TCHAR szIMEGUDHlpName[MAX_PATH];

			if (lpData == NULL)
				return FALSE;

			szIMEGUDHlpName[0] = 0;
			GetWindowsDirectory((LPTSTR) szIMEGUDHlpName, MAX_PATH);
			lstrcat((LPTSTR) szIMEGUDHlpName, TEXT("\\HELP\\WINGB.CHM"));

			lstrcpy((LPWSTR) lpData, szIMEGUDHlpName);

			return TRUE;

		}

	default:
		return (FALSE);
	}

	return (lRet);
}

/**********************************************************************/
/* InitCompStr()                                                      */
/**********************************************************************/
void PASCAL InitCompStr(		// init setting for composing string
						   LPCOMPOSITIONSTRING lpCompStr)
{
	if (!lpCompStr) {
		return;
	}

	lpCompStr->dwCompReadAttrLen = 0;
	lpCompStr->dwCompReadClauseLen = 0;
	lpCompStr->dwCompReadStrLen = 0;

	lpCompStr->dwCompAttrLen = 0;
	lpCompStr->dwCompClauseLen = 0;
	lpCompStr->dwCompStrLen = 0;

	lpCompStr->dwCursorPos = 0;
	lpCompStr->dwDeltaStart = 0;

	lpCompStr->dwResultReadClauseLen = 0;
	lpCompStr->dwResultReadStrLen = 0;

	lpCompStr->dwResultClauseLen = 0;
	lpCompStr->dwResultStrLen = 0;

	return;
}

/**********************************************************************/
/* ClearCompStr()                                                     */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL PASCAL ClearCompStr(LPINPUTCONTEXT lpIMC)
{
	HIMCC hMem;
	LPCOMPOSITIONSTRING lpCompStr;
	DWORD dwSize;

	if (!lpIMC) {
		return (FALSE);
	}

	dwSize =
		// header length
		sizeof(COMPOSITIONSTRING) +
		// composition reading attribute plus NULL terminator
		lpImeL->nMaxKey * sizeof(BYTE) + sizeof(BYTE) +
		// composition reading clause
		sizeof(DWORD) + sizeof(DWORD) +
		// composition reading string plus NULL terminator
		lpImeL->nMaxKey * sizeof(WORD) + sizeof(WORD) +
		// result reading clause
		sizeof(DWORD) + sizeof(DWORD) +
		// result reading string plus NULL terminateor
		lpImeL->nMaxKey * sizeof(WORD) + sizeof(WORD) +
		// result clause
		sizeof(DWORD) + sizeof(DWORD) +
		// result string plus NULL terminateor
		MAXSTRLEN * sizeof(WORD) + sizeof(WORD);

	if (!lpIMC->hCompStr) {
		// it maybe free by other IME, init it
		lpIMC->hCompStr = ImmCreateIMCC(dwSize);
	} else if (hMem = ImmReSizeIMCC(lpIMC->hCompStr, dwSize)) {
		lpIMC->hCompStr = hMem;
	} else {
		ImmDestroyIMCC(lpIMC->hCompStr);
		lpIMC->hCompStr = ImmCreateIMCC(dwSize);
		return (FALSE);
	}

	if (!lpIMC->hCompStr) {
		return (FALSE);
	}

	lpCompStr = (LPCOMPOSITIONSTRING) ImmLockIMCC(lpIMC->hCompStr);
	if (!lpCompStr) {
		ImmDestroyIMCC(lpIMC->hCompStr);
		lpIMC->hCompStr = ImmCreateIMCC(dwSize);
		return (FALSE);
	}

	lpCompStr->dwSize = dwSize;

	// 1. composition (reading) string - simple IME
	// 2. result reading string
	// 3. result string

	lpCompStr->dwCompReadAttrLen = 0;
	lpCompStr->dwCompReadAttrOffset = sizeof(COMPOSITIONSTRING);
	lpCompStr->dwCompReadClauseLen = 0;
	lpCompStr->dwCompReadClauseOffset = lpCompStr->dwCompReadAttrOffset +
		lpImeL->nMaxKey * sizeof(TCHAR) + sizeof(TCHAR);
	lpCompStr->dwCompReadStrLen = 0;
	lpCompStr->dwCompReadStrOffset = lpCompStr->dwCompReadClauseOffset +
		sizeof(DWORD) + sizeof(DWORD);

	// composition string is the same with composition reading string 
	// for simple IMEs
	lpCompStr->dwCompAttrLen = 0;
	lpCompStr->dwCompAttrOffset = lpCompStr->dwCompReadAttrOffset;
	lpCompStr->dwCompClauseLen = 0;
	lpCompStr->dwCompClauseOffset = lpCompStr->dwCompReadClauseOffset;
	lpCompStr->dwCompStrLen = 0;
	lpCompStr->dwCompStrOffset = lpCompStr->dwCompReadStrOffset;

	lpCompStr->dwCursorPos = 0;
	lpCompStr->dwDeltaStart = 0;

	lpCompStr->dwResultReadClauseLen = 0;
	lpCompStr->dwResultReadClauseOffset = lpCompStr->dwCompStrOffset +
		lpImeL->nMaxKey * sizeof(WORD) + sizeof(WORD);
	lpCompStr->dwResultReadStrLen = 0;
	lpCompStr->dwResultReadStrOffset =
		lpCompStr->dwResultReadClauseOffset + sizeof(DWORD) +
		sizeof(DWORD);

	lpCompStr->dwResultClauseLen = 0;
	lpCompStr->dwResultClauseOffset = lpCompStr->dwResultReadStrOffset +
		lpImeL->nMaxKey * sizeof(WORD) + sizeof(WORD);
	lpCompStr->dwResultStrOffset = 0;
	lpCompStr->dwResultStrOffset = lpCompStr->dwResultClauseOffset +
		sizeof(DWORD) + sizeof(DWORD);

	GlobalUnlock((HGLOBAL) lpIMC->hCompStr);
	return (TRUE);
}

/**********************************************************************/
/* ClearCand()                                                        */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL PASCAL ClearCand(LPINPUTCONTEXT lpIMC)
{
	HIMCC hMem;
	LPCANDIDATEINFO lpCandInfo;
	LPCANDIDATELIST lpCandList;
	DWORD dwSize =
		// header length
		sizeof(CANDIDATEINFO) + sizeof(CANDIDATELIST) +
		// candidate string pointers
		sizeof(DWORD) * (MAXCAND + 1) +
		// string plus NULL terminator
		(sizeof(WORD) + sizeof(WORD)) * (MAXCAND + 1);

	if (!lpIMC) {
		return (FALSE);
	}

	if (!lpIMC->hCandInfo) {
		// it maybe free by other IME, init it
		lpIMC->hCandInfo = ImmCreateIMCC(dwSize);
	} else if (hMem = ImmReSizeIMCC(lpIMC->hCandInfo, dwSize)) {
		lpIMC->hCandInfo = hMem;
	} else {
		ImmDestroyIMCC(lpIMC->hCandInfo);
		lpIMC->hCandInfo = ImmCreateIMCC(dwSize);
		return (FALSE);
	}

	if (!lpIMC->hCandInfo) {
		return (FALSE);
	}

	lpCandInfo = (LPCANDIDATEINFO) ImmLockIMCC(lpIMC->hCandInfo);
	if (!lpCandInfo) {
		ImmDestroyIMCC(lpIMC->hCandInfo);
		lpIMC->hCandInfo = ImmCreateIMCC(dwSize);
		return (FALSE);
	}
	// ordering of strings are
	// buffer size
	lpCandInfo->dwSize = dwSize;
	lpCandInfo->dwCount = 0;
	lpCandInfo->dwOffset[0] = sizeof(CANDIDATEINFO);
	lpCandList = (LPCANDIDATELIST) ((LPBYTE) lpCandInfo +
									lpCandInfo->dwOffset[0]);
	// whole candidate info size - header
	lpCandList->dwSize = lpCandInfo->dwSize - sizeof(CANDIDATEINFO);
	lpCandList->dwStyle = IME_CAND_READ;
	lpCandList->dwCount = 0;
	lpCandList->dwSelection = 0;
	lpCandList->dwPageSize = CANDPERPAGE;
	lpCandList->dwOffset[0] = sizeof(CANDIDATELIST) +
		sizeof(DWORD) * (MAXCAND);

	ImmUnlockIMCC(lpIMC->hCandInfo);
	return (TRUE);
}

/**********************************************************************/
/* ClearGuideLine()                                                   */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL PASCAL ClearGuideLine(LPINPUTCONTEXT lpIMC)
{
	HIMCC hMem;
	LPGUIDELINE lpGuideLine;
	DWORD dwSize = sizeof(GUIDELINE) + sImeG.cbStatusErr;

	if (!lpIMC->hGuideLine) {
		// it maybe free by IME
		lpIMC->hGuideLine = ImmCreateIMCC(dwSize);
	} else if (hMem = ImmReSizeIMCC(lpIMC->hGuideLine, dwSize)) {
		lpIMC->hGuideLine = hMem;
	} else {
		ImmDestroyIMCC(lpIMC->hGuideLine);
		lpIMC->hGuideLine = ImmCreateIMCC(dwSize);
	}

	lpGuideLine = (LPGUIDELINE) ImmLockIMCC(lpIMC->hGuideLine);
	if (!lpGuideLine) {
		return (FALSE);
	}

	lpGuideLine->dwSize = dwSize;
	lpGuideLine->dwLevel = GL_LEVEL_NOGUIDELINE;
	lpGuideLine->dwIndex = GL_ID_UNKNOWN;
	lpGuideLine->dwStrLen = 0;
	lpGuideLine->dwStrOffset = sizeof(GUIDELINE);

	CopyMemory((LPBYTE) lpGuideLine + lpGuideLine->dwStrOffset,
			   sImeG.szStatusErr, sImeG.cbStatusErr);

	ImmUnlockIMCC(lpIMC->hGuideLine);

	return (TRUE);
}

/**********************************************************************/
/* InitContext()                                                      */
/**********************************************************************/
void PASCAL InitContext(LPINPUTCONTEXT lpIMC)
{
	if (lpIMC->fdwInit & INIT_STATUSWNDPOS) {
	} else if (!lpIMC->hWnd) {
	} else {

		POINT ptWnd;

		ptWnd.x = 0;
		ptWnd.y = 0;
		ClientToScreen(lpIMC->hWnd, &ptWnd);

		if (ptWnd.x < sImeG.rcWorkArea.left) {
			lpIMC->ptStatusWndPos.x = sImeG.rcWorkArea.left;
		} else if (ptWnd.x + sImeG.xStatusWi > sImeG.rcWorkArea.right) {
			lpIMC->ptStatusWndPos.x =
				sImeG.rcWorkArea.right - sImeG.xStatusWi;
		} else {
			lpIMC->ptStatusWndPos.x = ptWnd.x;
		}

		lpIMC->ptStatusWndPos.y =
			sImeG.rcWorkArea.bottom - sImeG.yStatusHi;

		lpIMC->fdwInit |= INIT_STATUSWNDPOS;
	}

	if (lpIMC->fdwInit & INIT_COMPFORM) {
	} else if (!lpIMC->hWnd) {
	} else {
		POINT ptWnd;

		ptWnd = lpImeL->ptDefComp;
		ScreenToClient(lpIMC->hWnd, &ptWnd);
		lpIMC->cfCompForm.dwStyle = CFS_DEFAULT;
		lpIMC->cfCompForm.ptCurrentPos = ptWnd;
		lpIMC->fdwInit |= INIT_COMPFORM;
	}

	return;
}

/**********************************************************************/
/* Select()                                                           */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL PASCAL Select(HIMC hIMC, LPINPUTCONTEXT lpIMC, BOOL fSelect)
{
	LPPRIVCONTEXT lpImcP;

	if (fSelect) {

		if (!ClearCompStr(lpIMC))
			return FALSE;

		if (!ClearCand(lpIMC))
			return FALSE;

		ClearGuideLine(lpIMC);
	}

	if (lpIMC->cfCandForm[0].dwIndex != 0)
		lpIMC->cfCandForm[0].dwStyle = CFS_DEFAULT;

	// We add this hack for switching from other IMEs, this IME has a bug.
	// Before this bug fixed in this IME, it depends on this hack.
	if (lpIMC->cfCandForm[0].dwStyle == CFS_DEFAULT) {
		lpIMC->cfCandForm[0].dwIndex = (DWORD) - 1;
	}

	if (!lpIMC->hPrivate)
		return FALSE;

	lpImcP = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate);
	if (!lpImcP)
		return FALSE;

	if (fSelect) {
		//
		// init fields of hPrivate
		//
		lpImcP->iImeState = CST_INIT;
		lpImcP->fdwImeMsg = (DWORD) 0;
		lpImcP->dwCompChar = (DWORD) 0;
		lpImcP->fdwGcsFlag = (DWORD) 0;

		lpImcP->uSYHFlg = (UINT) 0;
		lpImcP->uDYHFlg = (UINT) 0;
		lpImcP->uDSMHCount = (UINT) 0;
		lpImcP->uDSMHFlg = (UINT) 0;

		//lpImcP->fdwSentence = (DWORD)NULL;

		*(LPDWORD) lpImcP->bSeq = 0;


		lpIMC->is_active = TRUE;

		if (!(lpIMC->fdwInit & INIT_CONVERSION)) {
			lpIMC->fdwConversion = IME_CMODE_NATIVE;
			lpIMC->fdwInit |= INIT_CONVERSION;
		}

		if (lpImeL->fModeConfig & MODE_CONFIG_PREDICT) {
			lpIMC->fdwSentence = IME_SMODE_PHRASEPREDICT;
			lpIMC->fdwInit |= INIT_SENTENCE;
		}

		if (!(lpIMC->fdwInit & INIT_LOGFONT)) {
			HDC hDC;
			HGDIOBJ hSysFont;

			//hSysFont = GetStockObject(SYSTEM_FONT);
			hDC = GetDC(NULL);
			hSysFont = GetCurrentObject(hDC, OBJ_FONT);
			GetObject(hSysFont, sizeof(LOGFONT), &lpIMC->lfFont.A);
			ReleaseDC(NULL, hDC);

			lpIMC->fdwInit |= INIT_LOGFONT;
		}

		InitContext(lpIMC);

		//
		// Set Caps status
		//
		{
			DWORD fdwConversion;

			if (GetKeyState(VK_CAPITAL) & 0x01) {

				//
				// Change to alphanumeric mode.
				//
				fdwConversion = lpIMC->fdwConversion &
					~(IME_CMODE_NATIVE | IME_CMODE_CHARCODE |
					  IME_CMODE_EUDC);
			} else {

				//
				// Change to native mode
				//
				fdwConversion = (lpIMC->fdwConversion | IME_CMODE_NATIVE) &
					~(IME_CMODE_CHARCODE | IME_CMODE_EUDC);
			}

			ImmSetConversionStatus(hIMC, fdwConversion,
								   lpIMC->fdwSentence);
		}

	} else {


		if (lpImeL->hPropMenu) {
			DestroyMenu(lpImeL->hPropMenu);
			lpImeL->hPropMenu = NULL;
		}

		if (hCrtDlg) {
			SendMessage(hCrtDlg, WM_CLOSE, (WPARAM) NULL, (LPARAM) NULL);
			hCrtDlg = NULL;
		}
	}

	ImmUnlockIMCC(lpIMC->hPrivate);

	return (TRUE);
}

/**********************************************************************/
/* ImeSelect()                                                        */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL WINAPI ImeSelect(HIMC hIMC, BOOL fSelect)
{
	LPINPUTCONTEXT lpIMC;
	BOOL fRet;


	if (!hIMC) {
		return (FALSE);
	}

	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
	if (!lpIMC) {
		return (FALSE);
	}

	fRet = Select(hIMC, lpIMC, fSelect);

	ImmUnlockIMC(hIMC);

	return (fRet);
}

/**********************************************************************/
/* ImeSetActiveContext()                                              */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL WINAPI ImeSetActiveContext(HIMC hIMC, BOOL fOn)
{
	if (!fOn) {
	} else if (!hIMC) {
	} else {
		LPINPUTCONTEXT lpIMC;

		lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
		if (!lpIMC) {
			return (FALSE);
		}

		InitContext(lpIMC);

		ImmUnlockIMC(hIMC);
	}

	return (TRUE);
}

/**********************************************************************/
//OpenReg_PathSetup(HKEY *phKey);
/**********************************************************************/
LONG OpenReg_PathSetup(HKEY * phKey)
{
	return RegOpenKeyEx(HKEY_CURRENT_USER,
						REGSTR_PATH_SETUP,
						0,
						KEY_ENUMERATE_SUB_KEYS |
						KEY_EXECUTE | KEY_QUERY_VALUE, phKey);
}

/**********************************************************************/
//LONG OpenReg_User(HKEY hKey,        // handle of open key 
//                LPCTSTR  lpszSubKey,    // address of name of subkey to open 
//                PHKEY  phkResult);     // address of handle of open key 
/**********************************************************************/
LONG OpenReg_User(HKEY hKey,	// handle of open key 
				  LPCTSTR lpszSubKey,	// address of name of subkey to open 
				  PHKEY phkResult)	// address of handle of open key 
{
	return RegOpenKeyEx(hKey, lpszSubKey, 0, KEY_ALL_ACCESS, phkResult);
}

VOID InfoMessage(HANDLE hWnd, WORD wMsgID)
{
	TCHAR szStr[256];

	LoadString(NULL, wMsgID, szStr, sizeof(szStr) / sizeof(TCHAR));
	MessageBox(hWnd, szStr, szWarnTitle, MB_ICONINFORMATION | MB_OK);
}

VOID FatalMessage(HANDLE hWnd, WORD wMsgID)
{
	TCHAR szStr[256];

	LoadString(NULL, wMsgID, szStr, sizeof(szStr) / sizeof(TCHAR));
	MessageBox(hWnd, szStr, szErrorTitle, MB_ICONSTOP | MB_OK);
}
