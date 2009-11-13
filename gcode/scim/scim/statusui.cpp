
/*++

  Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

  Module Name:

  statusui.c


  ++*/


#include <windows.h>
#include <immdev.h>
#include <htmlhelp.h>
#include <string.h>
#include <regstr.h>
#include <imedefs.h>
#include <resource.h>
#include "imewnd.h"
extern HWND hCrtDlg;

/**********************************************************************/
/* AdjustStatusBoundary()                                             */
/**********************************************************************/
void PASCAL AdjustStatusBoundary(LPPOINTS lppt, HWND hUIWnd)
{

	RECT rcWorkArea;

	rcWorkArea = get_wa_rect();

	// display boundary check
	if (lppt->x < rcWorkArea.left) {
		lppt->x = (short) rcWorkArea.left;
	} else if (lppt->x + sImeG.xStatusWi > rcWorkArea.right) {
		lppt->x = (short) (rcWorkArea.right - sImeG.xStatusWi);
	}

	if (lppt->y < rcWorkArea.top) {
		lppt->y = (short) rcWorkArea.top;
	} else if (lppt->y + sImeG.yStatusHi > rcWorkArea.bottom) {
		lppt->y = (short) (rcWorkArea.bottom - sImeG.yStatusHi);
	}

	return;
}

/**********************************************************************/
/* SetStatusWindowPos()                                               */
/**********************************************************************/
LRESULT PASCAL SetStatusWindowPos(HWND hStatusWnd)
{
	HWND hUIWnd;
	HIMC hIMC;
	LPINPUTCONTEXT lpIMC;
	POINTS ptPos;

	hUIWnd = GetWindow(hStatusWnd, GW_OWNER);

	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	if (!hIMC) {
		return (1L);
	}

	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
	if (!lpIMC) {				// Oh! Oh!
		return (1L);
	}

	ptPos.x = (short) lpIMC->ptStatusWndPos.x;
	ptPos.y = (short) lpIMC->ptStatusWndPos.y;

	// display boundary adjust
	AdjustStatusBoundary(&ptPos, hUIWnd);

	SetWindowPos(hStatusWnd, NULL,
				 ptPos.x, ptPos.y,
				 0, 0,
				 SWP_NOACTIVATE | SWP_NOCOPYBITS | SWP_NOSIZE |
				 SWP_NOZORDER);

	ImmUnlockIMC(hIMC);

	return (0L);
}

/**********************************************************************/
/* ShowStatus()                                                       */
/**********************************************************************/
void PASCAL ShowStatus(HWND hUIWnd, int nShowStatusCmd)
{
	ShowWindow(hStatusWnd, nShowStatusCmd);
	return;
}

/**********************************************************************/
/* OpenStatus()                                                       */
/**********************************************************************/
void PASCAL OpenStatus(			// open status window
						  HWND hUIWnd)
{
	HIMC hIMC;
	LPINPUTCONTEXT lpIMC;
	POINT ptPos;
	int nShowStatusCmd;
	RECT rcWorkArea;

	rcWorkArea = get_wa_rect();

	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	if (!hIMC) {
		ptPos.x = rcWorkArea.left;
		ptPos.y = rcWorkArea.bottom - sImeG.yStatusHi;
		nShowStatusCmd = SW_HIDE;
	} else if (lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC)) {


		if (lpIMC->ptStatusWndPos.x < rcWorkArea.left) {
			lpIMC->ptStatusWndPos.x = rcWorkArea.left;
		} else if (lpIMC->ptStatusWndPos.x + sImeG.xStatusWi >
				   rcWorkArea.right) {
			lpIMC->ptStatusWndPos.x = rcWorkArea.right - sImeG.xStatusWi;
		}

		if (lpIMC->ptStatusWndPos.y < rcWorkArea.top) {
			lpIMC->ptStatusWndPos.y = rcWorkArea.top;
		} else if (lpIMC->ptStatusWndPos.y + sImeG.yStatusHi >
				   rcWorkArea.right) {
			lpIMC->ptStatusWndPos.y = rcWorkArea.bottom - sImeG.yStatusHi;
		}

		ptPos.x = lpIMC->ptStatusWndPos.x;
		ptPos.y = lpIMC->ptStatusWndPos.y;

		ImmUnlockIMC(hIMC);
		nShowStatusCmd = SW_SHOWNOACTIVATE;
	} else {
		ptPos.x = rcWorkArea.left;
		ptPos.y = rcWorkArea.bottom - sImeG.yStatusHi;
		nShowStatusCmd = SW_HIDE;
	}

	if (hStatusWnd) {
		SetWindowPos(hStatusWnd, NULL,
					 ptPos.x, ptPos.y,
					 0, 0, SWP_NOACTIVATE | SWP_NOSIZE | SWP_NOZORDER);
	} else {					// create status window
		hStatusWnd =
			CreateWindowEx(WS_EX_TOPMOST, 
						   szStatusClassName, NULL, WS_POPUP | WS_DISABLED,
						   ptPos.x, ptPos.y, sImeG.xStatusWi,
						   sImeG.yStatusHi, hUIWnd, (HMENU) NULL, hInst,
						   NULL);
	}


	if (hIMC) {
		ShowStatus(hUIWnd, SW_SHOWNOACTIVATE);
	}

	return;
}

void PASCAL DestroyStatusWindow(HWND hStatusWnd)
{
	hStatusWnd = (HWND) NULL;
	return;
}

void PASCAL SetStatus(HWND hStatusWnd, LPPOINT lpptCursor)
{
	HWND hUIWnd;
	HIMC hIMC;
	LPINPUTCONTEXT lpIMC;

	hUIWnd = GetWindow(hStatusWnd, GW_OWNER);
	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	if (!hIMC) {
		return;
	}

	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
	if (!lpIMC) {
		return;
	}

	if (!lpIMC->fOpen) {
		ImmSetOpenStatus(hIMC, TRUE);
	} else if (PtInRect(&sImeG.rcImeIcon, *lpptCursor)) {
		DWORD fdwConversion;

		if (lpIMC->fdwConversion & (0 | IME_CMODE_EUDC)) {
			// change to native mode
			fdwConversion = (lpIMC->fdwConversion | IME_CMODE_NATIVE) &
				~(0 | IME_CMODE_EUDC);
		} else if (lpIMC->fdwConversion & IME_CMODE_NATIVE) {
			// change to alphanumeric mode
			fdwConversion = lpIMC->fdwConversion & ~(0 |
													 IME_CMODE_NATIVE |
													 IME_CMODE_EUDC);
		} else {


			BYTE lpbKeyState[256];

			GetKeyboardState(lpbKeyState);

			if (lpbKeyState[VK_CAPITAL] & 1) {
				// Simulate a key press
				keybd_event(VK_CAPITAL, 0x3A, KEYEVENTF_EXTENDEDKEY | 0,
							0);

				// Simulate a key release
				keybd_event(VK_CAPITAL,
							0x3A, KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP,
							0);
			}
			fdwConversion = (lpIMC->fdwConversion | IME_CMODE_NATIVE) &
				~(0 | IME_CMODE_EUDC);
			// 10.11 add
			uCaps = 0;
		}

		ImmSetConversionStatus(hIMC, fdwConversion, lpIMC->fdwSentence);
	} else if (PtInRect(&sImeG.rcImeName, *lpptCursor)) {
		DWORD dwConvMode;
		int cxBorder, cyBorder;

		//change current IME index
		dwConvMode =
			lpIMC->fdwConversion ^ (IME_CMODE_INDEX_FIRST << sImeL.
									dwRegImeIndex);
		sImeL.dwRegImeIndex = (sImeL.dwRegImeIndex + 1) % IMEINDEXNUM;
		szImeName = pszImeName[sImeL.dwRegImeIndex];
		dwConvMode |= (IME_CMODE_INDEX_FIRST << sImeL.dwRegImeIndex);

		// re-caculate statusuidata
		cxBorder = GetSystemMetrics(SM_CXBORDER);
		cyBorder = GetSystemMetrics(SM_CYBORDER);
		InitStatusUIData(cxBorder, cyBorder);

		ImmSetConversionStatus(hIMC, dwConvMode, lpIMC->fdwSentence);

		//set IME index in registry

	} else if (PtInRect(&sImeG.rcSymbol, *lpptCursor)) {
		DWORD fdwConversion;

		if (lpIMC->fdwConversion & 0) {
			MessageBeep((u32) - 1);
		} else {
			fdwConversion = lpIMC->fdwConversion ^ IME_CMODE_SYMBOL;
			ImmSetConversionStatus(hIMC, fdwConversion,
								   lpIMC->fdwSentence);
		}
	} else {
		MessageBeep((u32) - 1);
	}

	ImmUnlockIMC(hIMC);

	return;
}

void PASCAL PaintStatusWindow(HDC hDC, HWND hStatusWnd)
{
	HWND hUIWnd;
	HIMC hIMC;
	LPINPUTCONTEXT lpIMC;
	LPPRIVCONTEXT imcPrivPtr;
	HBITMAP hImeIconBmp, hSymbolBmp;
	HBITMAP hOldBmp;
	HDC hMemDC;

	hUIWnd = GetWindow(hStatusWnd, GW_OWNER);

	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	if (!hIMC) {
		MessageBeep((u32) - 1);
		return;
	}

	if (!(lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC))) {
		MessageBeep((u32) - 1);
		return;
	}
	// get imcPrivPtr
	if (!(imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate))) {
		MessageBeep((u32) - 1);
		return;
	}
	//in case the IME index has been changed and the ImeName size is different
	{
		POINTS ptPos;

		ptPos.x = (short) lpIMC->ptStatusWndPos.x;
		ptPos.y = (short) lpIMC->ptStatusWndPos.y;

		SetWindowPos(hStatusWnd, NULL,
					 ptPos.x, ptPos.y,
					 sImeG.xStatusWi, sImeG.yStatusHi,
					 SWP_NOACTIVATE | SWP_NOCOPYBITS | SWP_NOZORDER);
	}


	if (lpIMC->fOpen) {
		SetTextColor(hDC, RGB(0x00, 0x00, 0x00));
	} else {
		SetTextColor(hDC, RGB(0x80, 0x80, 0x80));
	}

	SetBkColor(hDC, RGB(0xC0, 0xC0, 0xC0));
	DrawText(hDC, szImeName, lstrlen(szImeName),
			 &sImeG.rcImeName, DT_CENTER | DT_VCENTER | DT_SINGLELINE);

	// load all bitmap
	hSymbolBmp = (HBITMAP) NULL;

	if (!lpIMC->fOpen) {
		hSymbolBmp = LoadBitmap(hInst, szNone);
		hImeIconBmp = LoadBitmap(hInst, szChinese);
	} else if (lpIMC->fdwConversion & IME_CMODE_NATIVE) {
		hImeIconBmp = LoadBitmap(hInst, szChinese);
	} else {
		hImeIconBmp = LoadBitmap(hInst, szEnglish);
	}

	if (!hSymbolBmp) {
		if (lpIMC->fdwConversion & IME_CMODE_SYMBOL) {
			hSymbolBmp = LoadBitmap(hInst, szSymbol);
		} else {
			hSymbolBmp = LoadBitmap(hInst, szNoSymbol);
		}
	}


	ImmUnlockIMC(hIMC);
	ImmUnlockIMCC(lpIMC->hPrivate);

	hMemDC = CreateCompatibleDC(hDC);

	hOldBmp = SelectObject(hMemDC, hImeIconBmp);

	BitBlt(hDC, sImeG.rcImeIcon.left, sImeG.rcImeIcon.top,
		   sImeG.rcImeIcon.right - sImeG.rcImeIcon.left,
		   STATUS_DIM_Y, hMemDC, 0, 0, SRCCOPY);

	SelectObject(hMemDC, hSymbolBmp);

	BitBlt(hDC, sImeG.rcSymbol.left, sImeG.rcSymbol.top,
		   sImeG.rcSymbol.right - sImeG.rcSymbol.left,
		   STATUS_DIM_Y, hMemDC, 0, 0, SRCCOPY);



	SelectObject(hMemDC, hOldBmp);

	DeleteDC(hMemDC);

	DeleteObject(hImeIconBmp);
	DeleteObject(hSymbolBmp);

	return;
}

/**********************************************************************/
/* StatusWndProc()                                                    */
/**********************************************************************/
LRESULT CALLBACK
StatusWndProc(HWND hStatusWnd, u32 uMsg, WPARAM wParam, LPARAM lParam)
{
	//BHJDEBUG("received msg %s", msg_name(uMsg));
	switch (uMsg) {
	case WM_DESTROY:
		DestroyStatusWindow(hStatusWnd);
		break;
	case WM_SETCURSOR:
		break;
	case WM_MOUSEMOVE:
		break;
	case WM_LBUTTONUP:
		break;

	case WM_IME_NOTIFY:
		// get work area for changing
		SystemParametersInfo(SPI_GETWORKAREA, 0, &get_wa_rect(), 0);

		if (wParam == IMN_SETSTATUSWINDOWPOS) {
			SetStatusWindowPos(hStatusWnd);
		}
		break;
	case WM_PAINT:
		{
			HDC hDC;
			PAINTSTRUCT ps;

			hDC = BeginPaint(hStatusWnd, &ps);
			PaintStatusWindow(hDC, hStatusWnd);
			EndPaint(hStatusWnd, &ps);
		}
		break;
	case WM_MOUSEACTIVATE:
		return (MA_NOACTIVATE);
	default:
		//BHJDEBUG(" msg %s not handled", msg_name(uMsg));
		return DefWindowProc(hStatusWnd, uMsg, wParam, lParam);
	}

	return (0L);
}

