
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

void PASCAL GetNearCaretPosition(LPPOINT lpptFont,
								 u32 uEsc,
								 u32 uRot,
								 LPPOINT lpptCaret,
								 LPPOINT lpptNearCaret, 
								 BOOL fFlags)
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

	rcWorkArea = get_wa_rect();

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

BOOL PASCAL AdjustCompPosition(
	input_context& ic, 
	LPPOINT lpptOrg, 
	LPPOINT lpptNew)
{
	POINT ptNearCaret, ptOldNearCaret;
	u32 uEsc, uRot;
	RECT rcUIRect, rcInputRect, rcInterRect;
	POINT ptFont;

	// we need to adjust according to font attribute
	if (ic->lfFont.A.lfWidth > 0) {
		ptFont.x = ic->lfFont.A.lfWidth * 2;
	} else if (ic->lfFont.A.lfWidth < 0) {
		ptFont.x = -ic->lfFont.A.lfWidth * 2;
	} else if (ic->lfFont.A.lfHeight > 0) {
		ptFont.x = ic->lfFont.A.lfHeight;
	} else if (ic->lfFont.A.lfHeight < 0) {
		ptFont.x = -ic->lfFont.A.lfHeight;
	} else {
		ptFont.x = lpImeL->yCompHi;
	}

	if (ic->lfFont.A.lfHeight > 0) {
		ptFont.y = ic->lfFont.A.lfHeight;
	} else if (ic->lfFont.A.lfHeight < 0) {
		ptFont.y = -ic->lfFont.A.lfHeight;
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
	uEsc = (u32) ((ic->lfFont.A.lfEscapement + 450) / 900 % 4);
	uRot = (u32) ((ic->lfFont.A.lfOrientation + 450) / 900 % 4);

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
							   input_context& ic)
{
	return;
	POINT ptWnd;
	BOOL fChange = FALSE;
	RECT rcWorkArea;

	rcWorkArea = get_wa_rect();

	// the client coordinate position (0, 0) of composition window
	ptWnd.x = 0;
	ptWnd.y = 0;
	// convert to screen coordinates
	ClientToScreen(hCompWnd, &ptWnd);
	ptWnd.x -= lpImeL->cxCompBorder;
	ptWnd.y -= lpImeL->cyCompBorder;

	if (ic->cfCompForm.dwStyle & CFS_FORCE_POSITION) {
		POINT ptNew;			// new position of UI

		ptNew.x = ic->cfCompForm.ptCurrentPos.x;
		ptNew.y = ic->cfCompForm.ptCurrentPos.y;
		ClientToScreen((HWND) ic->hWnd, &ptNew);
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
	} else if (ic->cfCompForm.dwStyle != CFS_DEFAULT) {
		POINT ptNew;			// new position of UI

		ptNew.x = ic->cfCompForm.ptCurrentPos.x;
		ptNew.y = ic->cfCompForm.ptCurrentPos.y;
		ClientToScreen((HWND) ic->hWnd, &ptNew);
		fChange = AdjustCompPosition(ic, &ptWnd, &ptNew);
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

void PASCAL MoveDefaultCompPosition(	// the default comp position
									   // need to near the caret
									   HWND hUIWnd)
{
	HIMC hIMC;
	
	HWND hCompWnd;

	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);

	hCompWnd = GetCompWnd(hUIWnd);
	if (!hCompWnd) {
		return;
	}

	input_context ic(hIMC);
	if (!ic) {
		return;
	}

	if (!(ic->cfCompForm.dwStyle & CFS_FORCE_POSITION)) {
		SetCompPosition(hCompWnd, hIMC, ic);
	}

	

	return;
}

void PASCAL ShowComp(int nShowCmd)
{
	ShowWindow(hCompWnd, nShowCmd);
	return;
}

void PASCAL StartComp(HWND hUIWnd)
{
	EnterLeaveDebug(); 
	HIMC hIMC;
	

	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);

	input_context ic(hIMC);
	if (!ic) {
		return;
	}

	if (!hCompWnd) {
		hCompWnd = CreateWindowEx(0, szCompClassName, NULL, WS_POPUP | WS_DISABLED,
								  0, 0, 400, 60, hUIWnd,
								  (HMENU) NULL, hInst, NULL);
	}

	SetCompPosition(hCompWnd, hIMC, ic);
	ShowComp(SW_SHOWNOACTIVATE);

	return;
}

/**********************************************************************/
/* EndComp()                                                          */
/**********************************************************************/
void PASCAL EndComp(HWND hUIWnd)
{
	ShowComp(SW_HIDE);

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

void PASCAL PaintCompWindow(HWND hCompWnd, HDC hDC)
{
	EnterLeaveDebug(); 
	BHJDEBUG(" g_comp_str is %s", g_comp_str.c_str());

	RECT rcWnd;
	GetClientRect(hCompWnd, &rcWnd);

	SetBkColor(hDC, RGB(255, 255, 255));

	if (g_comp_str.size()) {
		wstring wstr = to_wstring(g_comp_str);
		ExtTextOut(hDC, 10, 1, 0, 0, wstr.c_str(), wstr.size(), NULL);
		BHJDEBUG(" g_comp_str size is %d, wstr size is %d", g_comp_str.size(), wstr.size());
	} 
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
		BHJDEBUG(" wm_ime_notify wp %x, lp %x", wParam, lParam);
		// must not delete this case, because DefWindowProc will hang the IME
		break;
	case WM_PAINT:
		{
			HDC hDC;
			PAINTSTRUCT ps;

			hDC = BeginPaint(hCompWnd, &ps);
			PaintCompWindow(hCompWnd, hDC);
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
