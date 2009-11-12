
/*++

Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

Module Name:

    compui.c

++*/


#include <windows.h>
#include <immdev.h>
#include "imedefs.h"
#include <regstr.h>
#include "imewnd.h"

/**********************************************************************/
/* GetCompWnd                                                         */
/* Return Value :                                                     */
/*      window handle of composition                                  */
/**********************************************************************/
extern "C" HWND PASCAL GetCompWnd(HWND hUIWnd)	// UI window
{
	return (hCompWnd);
}

BOOL PASCAL FitInLazyOperation(	// fit in lazy operation or not
								  LPPOINT lpptOrg, LPPOINT lpptNearCaret,	// the suggested near caret position
								  LPRECT lprcInputRect, u32 uEsc)
{
	return false;
	POINT ptDelta, ptTol;
	RECT rcUIRect, rcInterRect;

	ptDelta.x = lpptOrg->x - lpptNearCaret->x;

	ptDelta.x = (ptDelta.x >= 0) ? ptDelta.x : -ptDelta.x;

	ptTol.x = sImeG.iParaTol * ncUIEsc[uEsc].iParaFacX +
		sImeG.iPerpTol * ncUIEsc[uEsc].iPerpFacX;

	ptTol.x = (ptTol.x >= 0) ? ptTol.x : -ptTol.x;

	if (ptDelta.x > ptTol.x) {
		return FALSE;
	}

	ptDelta.y = lpptOrg->y - lpptNearCaret->y;

	ptDelta.y = (ptDelta.y >= 0) ? ptDelta.y : -ptDelta.y;

	ptTol.y = sImeG.iParaTol * ncUIEsc[uEsc].iParaFacY +
		sImeG.iPerpTol * ncUIEsc[uEsc].iPerpFacY;

	ptTol.y = (ptTol.y >= 0) ? ptTol.y : -ptTol.y;

	if (ptDelta.y > ptTol.y) {
		return FALSE;
	}
	// build up the UI rectangle (composition window)
	rcUIRect.left = lpptOrg->x;
	rcUIRect.top = lpptOrg->y;
	rcUIRect.right = rcUIRect.left + lpImeL->xCompWi;
	rcUIRect.bottom = rcUIRect.top + lpImeL->yCompHi;

	if (IntersectRect(&rcInterRect, &rcUIRect, lprcInputRect)) {
		return FALSE;
	}

	return (TRUE);
}

/**********************************************************************/
/* GetNearCaretPosition()                                             */
/**********************************************************************/
void PASCAL GetNearCaretPosition(	// decide a near caret position according
									// to the caret position
									LPPOINT lpptFont,
									u32 uEsc,
									u32 uRot,
									LPPOINT lpptCaret,
									LPPOINT lpptNearCaret, BOOL fFlags)
{
	LONG lFontSize;
	LONG xWidthUI, yHeightUI, xBorder, yBorder;
	RECT rcWorkArea;

	if ((uEsc + uRot) & 0x0001) {
		lFontSize = lpptFont->x;
	} else {
		lFontSize = lpptFont->y;
	}

	xWidthUI = lpImeL->xCompWi;
	yHeightUI = lpImeL->yCompHi;
	xBorder = lpImeL->cxCompBorder;
	yBorder = lpImeL->cyCompBorder;

	if (fFlags & NEAR_CARET_FIRST_TIME) {
		lpptNearCaret->x = lpptCaret->x +
			lFontSize * ncUIEsc[uEsc].iLogFontFacX +
			sImeG.iPara * ncUIEsc[uEsc].iParaFacX +
			sImeG.iPerp * ncUIEsc[uEsc].iPerpFacX;

		if (ptInputEsc[uEsc].x >= 0) {
			lpptNearCaret->x += xBorder * 2;
		} else {
			lpptNearCaret->x -= xWidthUI - xBorder * 2;
		}

		lpptNearCaret->y = lpptCaret->y +
			lFontSize * ncUIEsc[uEsc].iLogFontFacY +
			sImeG.iPara * ncUIEsc[uEsc].iParaFacY +
			sImeG.iPerp * ncUIEsc[uEsc].iPerpFacY;

		if (ptInputEsc[uEsc].y >= 0) {
			lpptNearCaret->y += yBorder * 2;
		} else {
			lpptNearCaret->y -= yHeightUI - yBorder * 2;
		}
	} else {
		lpptNearCaret->x = lpptCaret->x +
			lFontSize * ncAltUIEsc[uEsc].iLogFontFacX +
			sImeG.iPara * ncAltUIEsc[uEsc].iParaFacX +
			sImeG.iPerp * ncAltUIEsc[uEsc].iPerpFacX;

		if (ptAltInputEsc[uEsc].x >= 0) {
			lpptNearCaret->x += xBorder * 2;
		} else {
			lpptNearCaret->x -= xWidthUI - xBorder * 2;
		}

		lpptNearCaret->y = lpptCaret->y +
			lFontSize * ncAltUIEsc[uEsc].iLogFontFacY +
			sImeG.iPara * ncAltUIEsc[uEsc].iParaFacY +
			sImeG.iPerp * ncAltUIEsc[uEsc].iPerpFacY;

		if (ptAltInputEsc[uEsc].y >= 0) {
			lpptNearCaret->y += yBorder * 2;
		} else {
			lpptNearCaret->y -= yHeightUI - yBorder * 2;
		}
	}

	rcWorkArea = sImeG.rcWorkArea;

	if (lpptNearCaret->x < rcWorkArea.left) {
		lpptNearCaret->x = rcWorkArea.left;
	} else if (lpptNearCaret->x + xWidthUI > rcWorkArea.right) {
		lpptNearCaret->x = rcWorkArea.right - xWidthUI;
	}

	if (lpptNearCaret->y < rcWorkArea.top) {
		lpptNearCaret->y = rcWorkArea.top;
	} else if (lpptNearCaret->y + yHeightUI > rcWorkArea.bottom) {
		lpptNearCaret->y = rcWorkArea.bottom - yHeightUI;
	}

	return;
}

/**********************************************************************/
/* AdjustCompPosition()                                               */
/* Return Value :                                                     */
/*      the position of composition window is changed or not          */
/**********************************************************************/
BOOL PASCAL AdjustCompPosition(	// IME adjust position according to
								  // composition form
								  LPINPUTCONTEXT lpIMC, LPPOINT lpptOrg,	// original composition window
								  // and final position
								  LPPOINT lpptNew)	// new expect position
{
	POINT ptNearCaret, ptOldNearCaret;
	u32 uEsc, uRot;
	RECT rcUIRect, rcInputRect, rcInterRect;
	POINT ptFont;

	// we need to adjust according to font attribute
	if (lpIMC->lfFont.A.lfWidth > 0) {
		ptFont.x = lpIMC->lfFont.A.lfWidth * 2;
	} else if (lpIMC->lfFont.A.lfWidth < 0) {
		ptFont.x = -lpIMC->lfFont.A.lfWidth * 2;
	} else if (lpIMC->lfFont.A.lfHeight > 0) {
		ptFont.x = lpIMC->lfFont.A.lfHeight;
	} else if (lpIMC->lfFont.A.lfHeight < 0) {
		ptFont.x = -lpIMC->lfFont.A.lfHeight;
	} else {
		ptFont.x = lpImeL->yCompHi;
	}

	if (lpIMC->lfFont.A.lfHeight > 0) {
		ptFont.y = lpIMC->lfFont.A.lfHeight;
	} else if (lpIMC->lfFont.A.lfHeight < 0) {
		ptFont.y = -lpIMC->lfFont.A.lfHeight;
	} else {
		ptFont.y = ptFont.x;
	}

	// if the input char is too big, we don't need to consider so much
	if (ptFont.x > lpImeL->yCompHi * 8) {
		ptFont.x = lpImeL->yCompHi * 8;
	}
	if (ptFont.y > lpImeL->yCompHi * 8) {
		ptFont.y = lpImeL->yCompHi * 8;
	}

	if (ptFont.x < sImeG.xChiCharWi) {
		ptFont.x = sImeG.xChiCharWi;
	}

	if (ptFont.y < sImeG.yChiCharHi) {
		ptFont.y = sImeG.yChiCharHi;
	}
	// -450 to 450 index 0
	// 450 to 1350 index 1
	// 1350 to 2250 index 2
	// 2250 to 3150 index 3
	uEsc = (u32) ((lpIMC->lfFont.A.lfEscapement + 450) / 900 % 4);
	uRot = (u32) ((lpIMC->lfFont.A.lfOrientation + 450) / 900 % 4);

	// decide the input rectangle
	rcInputRect.left = lpptNew->x;
	rcInputRect.top = lpptNew->y;

	// build up an input rectangle from escapemment
	rcInputRect.right = rcInputRect.left + ptFont.x * ptInputEsc[uEsc].x;
	rcInputRect.bottom = rcInputRect.top + ptFont.y * ptInputEsc[uEsc].y;

	// be a normal rectangle, not a negative rectangle
	if (rcInputRect.left > rcInputRect.right) {
		LONG tmp;

		tmp = rcInputRect.left;
		rcInputRect.left = rcInputRect.right;
		rcInputRect.right = tmp;
	}

	if (rcInputRect.top > rcInputRect.bottom) {
		LONG tmp;

		tmp = rcInputRect.top;
		rcInputRect.top = rcInputRect.bottom;
		rcInputRect.bottom = tmp;
	}

	GetNearCaretPosition(&ptFont, uEsc, uRot, lpptNew, &ptNearCaret,
						 NEAR_CARET_FIRST_TIME);

	// 1st, use the adjust point
	// build up the new suggest UI rectangle (composition window)
	rcUIRect.left = ptNearCaret.x;
	rcUIRect.top = ptNearCaret.y;
	rcUIRect.right = rcUIRect.left + lpImeL->xCompWi;
	rcUIRect.bottom = rcUIRect.top + lpImeL->yCompHi;

	ptOldNearCaret = ptNearCaret;

	// OK, no intersect between the near caret position and input char
	if (IntersectRect(&rcInterRect, &rcUIRect, &rcInputRect)) {
	} else
		if (FitInLazyOperation(lpptOrg, &ptNearCaret, &rcInputRect, uEsc))
	{
		// happy ending!!!, don't change position
		return FALSE;
	} else {
		*lpptOrg = ptNearCaret;

		// happy ending!!
		return (TRUE);
	}

	// unhappy case
	GetNearCaretPosition(&ptFont, uEsc, uRot, lpptNew, &ptNearCaret, 0);

	// build up the new suggest UI rectangle (composition window)
	rcUIRect.left = ptNearCaret.x;
	rcUIRect.top = ptNearCaret.y;
	rcUIRect.right = rcUIRect.left + lpImeL->xCompWi;
	rcUIRect.bottom = rcUIRect.top + lpImeL->yCompHi;

	// OK, no intersect between the adjust position and input char
	if (IntersectRect(&rcInterRect, &rcUIRect, &rcInputRect)) {
	} else
		if (FitInLazyOperation(lpptOrg, &ptNearCaret, &rcInputRect, uEsc))
	{
		return FALSE;
	} else {
		*lpptOrg = ptNearCaret;

		return (TRUE);
	}

	*lpptOrg = ptOldNearCaret;

	return (TRUE);
}

void PASCAL SetCompPosition(	// set the composition window position
							   HWND hCompWnd, HIMC hIMC,
							   LPINPUTCONTEXT lpIMC)
{
	return;
	POINT ptWnd;
	BOOL fChange = FALSE;
	RECT rcWorkArea;

	rcWorkArea = sImeG.rcWorkArea;

	// the client coordinate position (0, 0) of composition window
	ptWnd.x = 0;
	ptWnd.y = 0;
	// convert to screen coordinates
	ClientToScreen(hCompWnd, &ptWnd);
	ptWnd.x -= lpImeL->cxCompBorder;
	ptWnd.y -= lpImeL->cyCompBorder;

	if (lpIMC->cfCompForm.dwStyle & CFS_FORCE_POSITION) {
		POINT ptNew;			// new position of UI

		ptNew.x = lpIMC->cfCompForm.ptCurrentPos.x;
		ptNew.y = lpIMC->cfCompForm.ptCurrentPos.y;
		ClientToScreen((HWND) lpIMC->hWnd, &ptNew);
		if (ptWnd.x != ptNew.x) {
			ptWnd.x = ptNew.x;
			fChange = TRUE;
		}
		if (ptWnd.y != ptNew.y) {
			ptWnd.y = ptNew.y;
			fChange = TRUE;
		}
		if (fChange) {
			ptWnd.x -= lpImeL->cxCompBorder;
			ptWnd.y -= lpImeL->cyCompBorder;
		}
	} else if (lpIMC->cfCompForm.dwStyle != CFS_DEFAULT) {
		POINT ptNew;			// new position of UI

		ptNew.x = lpIMC->cfCompForm.ptCurrentPos.x;
		ptNew.y = lpIMC->cfCompForm.ptCurrentPos.y;
		ClientToScreen((HWND) lpIMC->hWnd, &ptNew);
		fChange = AdjustCompPosition(lpIMC, &ptWnd, &ptNew);
	} else {
		//fixme
		BHJDEBUG(" fixme");
	}

	if (!fChange) {
		return;
	}
	SetWindowPos(hCompWnd, NULL,
				 ptWnd.x, ptWnd.y,
				 0, 0, SWP_NOACTIVATE | SWP_NOSIZE | SWP_NOZORDER);

	return;
}

void PASCAL SetCompWindow(HWND hCompWnd)
{
	HIMC hIMC;
	LPINPUTCONTEXT lpIMC;
	HWND hUIWnd;

	hUIWnd = GetWindow(hCompWnd, GW_OWNER);
	if (!hUIWnd) {
		return;
	}
	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	if (!hIMC) {
		return;
	}

	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
	if (!lpIMC) {
		return;
	}

	SetCompPosition(hCompWnd, hIMC, lpIMC);

	ImmUnlockIMC(hIMC);

	return;
}

/**********************************************************************/
/* MoveDefaultCompPosition()                                          */
/**********************************************************************/
void PASCAL MoveDefaultCompPosition(	// the default comp position
									   // need to near the caret
									   HWND hUIWnd)
{
	HIMC hIMC;
	LPINPUTCONTEXT lpIMC;
	HWND hCompWnd;

	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	if (!hIMC) {
		return;
	}

	hCompWnd = GetCompWnd(hUIWnd);
	if (!hCompWnd) {
		return;
	}

	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
	if (!lpIMC) {
		return;
	}

	if (!(lpIMC->cfCompForm.dwStyle & CFS_FORCE_POSITION)) {
		SetCompPosition(hCompWnd, hIMC, lpIMC);
	}

	ImmUnlockIMC(hIMC);

	return;
}

void PASCAL ShowComp(			// Show the composition window
						HWND hUIWnd, int nShowCompCmd)
{
	ShowWindow(hCompWnd, nShowCompCmd);
	return;
}

void PASCAL StartComp(HWND hUIWnd)
{
	EnterLeaveDebug(); 
	HIMC hIMC;
	LPINPUTCONTEXT lpIMC;

	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	if (!hIMC) {
		return;
	}

	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
	if (!lpIMC) {
		return;
	}

	if (!hCompWnd) {
		hCompWnd =
			CreateWindowEx(WS_EX_TOPMOST,
						   szCompClassName, NULL, WS_POPUP | WS_DISABLED,
						   0, 0, 400, 60, hUIWnd,
						   (HMENU) NULL, hInst, NULL);
	}

	SetCompPosition(hCompWnd, hIMC, lpIMC);

	ImmUnlockIMC(hIMC);

	ShowComp(hUIWnd, SW_SHOWNOACTIVATE);

	return;
}

/**********************************************************************/
/* EndComp()                                                          */
/**********************************************************************/
void PASCAL EndComp(HWND hUIWnd)
{
	ShowComp(hUIWnd, SW_HIDE);

	return;
}

/**********************************************************************/
/* DestroyCompWindow()                                                */
/**********************************************************************/
void PASCAL DestroyCompWindow(	// destroy composition window
								 HWND hCompWnd)
{
	hCompWnd = (HWND) NULL;
	return;
}

void PASCAL PaintCompWindow(HWND hUIWnd, HWND hCompWnd, HDC hDC)
{
	EnterLeaveDebug(); 
	BHJDEBUG(" g_comp_str is %s", g_comp_str.c_str());
	HIMC hIMC;
	LPINPUTCONTEXT lpIMC;
	LPCOMPOSITIONSTRING lpCompStr;

	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	if (!hIMC) {
		return;
	}

	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
	if (!lpIMC) {
		return;
	}

	lpCompStr = (LPCOMPOSITIONSTRING) ImmLockIMCC(lpIMC->hCompStr);
	if (!lpCompStr) {
		return;
	}


	RECT rcWnd;
	GetClientRect(hCompWnd, &rcWnd);
	debug_rect(rcWnd);

	//FillSolidRect(hDC, rcWnd, RGB(255, 255, 255));


	SetBkColor(hDC, RGB(255, 255, 255));

	if (g_comp_str.size()) {
		wstring wstr = to_wstring(g_comp_str);
		ExtTextOut(hDC, 10, 1, 0, 0, wstr.c_str(), wstr.size(), NULL);
		BHJDEBUG(" g_comp_str size is %d, wstr size is %d", g_comp_str.size(), wstr.size());
	} 


	LPCANDIDATEINFO lpCandInfo;
	LPCANDIDATELIST lpCandList;
	LPPRIVCONTEXT imcPrivPtr;
	DWORD dwStart, dwEnd;
	TCHAR szStrBuf[2 * MAXSTRLEN * sizeof(WCHAR) / sizeof(TCHAR) + 1];
	int i;


	if (!lpIMC->hCandInfo) {
		BHJDEBUG(" no candinfo");
		goto UpCandW2UnlockIMC;
	}

	lpCandInfo = (LPCANDIDATEINFO) ImmLockIMCC(lpIMC->hCandInfo);
	if (!lpCandInfo) {
		goto UpCandW2UnlockIMC;
	}

	if (!lpIMC->hPrivate) {
		goto UpCandW2UnlockCandInfo;
	}

	imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate);
	if (!imcPrivPtr) {
		goto UpCandW2UnlockCandInfo;
	}
	// set font
	lpCandList = (LPCANDIDATELIST) ((LPBYTE) lpCandInfo +
									lpCandInfo->dwOffset[0]);

	dwStart = lpCandList->dwSelection;
	dwEnd = dwStart + lpCandList->dwPageSize;

	if (dwEnd > lpCandList->dwCount) {
		dwEnd = lpCandList->dwCount;
	}

	szStrBuf[0] = TEXT('1');
	szStrBuf[1] = TEXT(':');

	for (i = 0; dwStart < dwEnd; dwStart++, i++) {
		BHJDEBUG(" I'm gono draw cand in comp");

		szStrBuf[0] = szDigit[i + CAND_START];


		WORD wCode;
		wCode =
			*(LPUNAWORD) ((LPBYTE) lpCandList +
						  lpCandList->dwOffset[dwStart]);

		szStrBuf[2] = wCode;
		szStrBuf[3] = TEXT('\0');

		BHJDEBUG("szStrBuf is %04x %d", wCode, i);


		ExtTextOut(hDC, i * 40,
				   20,
				   0, NULL, szStrBuf, 3, NULL);

	}




	ImmUnlockIMCC(lpIMC->hPrivate);
UpCandW2UnlockCandInfo:
	ImmUnlockIMCC(lpIMC->hCandInfo);
UpCandW2UnlockIMC:

	ImmUnlockIMCC(lpIMC->hGuideLine);
	ImmUnlockIMCC(lpIMC->hCompStr);
	ImmUnlockIMC(hIMC);
	return;
}

LRESULT CALLBACK CompWndProc(	// composition window proc
								HWND hCompWnd, u32 uMsg, WPARAM wParam,
								LPARAM lParam)
{
	//BHJDEBUG("received msg %s", msg_name(uMsg));
	switch (uMsg) {
	case WM_DESTROY:
		DestroyCompWindow(hCompWnd);
		break;
	case WM_SETCURSOR:
		break;
	case WM_MOUSEMOVE:
		break;
	case WM_LBUTTONUP:
		break;
	case WM_IME_NOTIFY:
		if (wParam != IMN_SETCOMPOSITIONWINDOW) {
		} else {
			SetCompWindow(hCompWnd);
		}
		break;
	case WM_PAINT:
		{
			HDC hDC;
			PAINTSTRUCT ps;

			hDC = BeginPaint(hCompWnd, &ps);
			PaintCompWindow(GetWindow(hCompWnd, GW_OWNER), hCompWnd, hDC);
			EndPaint(hCompWnd, &ps);
		}
		break;
	case WM_MOUSEACTIVATE:
		return (MA_NOACTIVATE);
	default:
		//BHJDEBUG(" msg %s not handled", msg_name(uMsg));
		return DefWindowProc(hCompWnd, uMsg, wParam, lParam);
	}
	return (0L);
}
