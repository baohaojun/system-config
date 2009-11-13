
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

LRESULT PASCAL SetStatusWindowPos(HWND hUIWnd)
{
	HIMC hIMC;
	
	POINTS ptPos;
	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);

	input_context ic(hIMC);
	if (!ic) {
		return (1L);
	}

	ptPos.x = (short) ic->ptStatusWndPos.x;
	ptPos.y = (short) ic->ptStatusWndPos.y;

	// display boundary adjust
	AdjustStatusBoundary(&ptPos, hUIWnd);

	SetWindowPos(hStatusWnd, NULL,
				 ptPos.x, ptPos.y,
				 0, 0,
				 SWP_NOACTIVATE | SWP_NOCOPYBITS | SWP_NOSIZE |
				 SWP_NOZORDER);

	

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
	
	POINT ptPos;
	int nShowStatusCmd;
	RECT rcWorkArea;

	rcWorkArea = get_wa_rect();

	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	input_context ic(hIMC);

	if (!ic) {
		ptPos.x = rcWorkArea.left;
		ptPos.y = rcWorkArea.bottom - sImeG.yStatusHi;
		nShowStatusCmd = SW_HIDE;
	} else {


		if (ic->ptStatusWndPos.x < rcWorkArea.left) {
			ic->ptStatusWndPos.x = rcWorkArea.left;
		} else if (ic->ptStatusWndPos.x + sImeG.xStatusWi >
				   rcWorkArea.right) {
			ic->ptStatusWndPos.x = rcWorkArea.right - sImeG.xStatusWi;
		}

		if (ic->ptStatusWndPos.y < rcWorkArea.top) {
			ic->ptStatusWndPos.y = rcWorkArea.top;
		} else if (ic->ptStatusWndPos.y + sImeG.yStatusHi >
				   rcWorkArea.right) {
			ic->ptStatusWndPos.y = rcWorkArea.bottom - sImeG.yStatusHi;
		}

		ptPos.x = ic->ptStatusWndPos.x;
		ptPos.y = ic->ptStatusWndPos.y;

		
		nShowStatusCmd = SW_SHOWNOACTIVATE;
	} 

	if (hStatusWnd) {
		SetWindowPos(hStatusWnd, NULL,
					 ptPos.x, ptPos.y,
					 0, 0, SWP_NOACTIVATE | SWP_NOSIZE | SWP_NOZORDER);
	} else {					// create status window
		hStatusWnd =
			CreateWindowEx(0,
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

static void DestroyStatusWindow()
{
	hStatusWnd = NULL;
	return;
}

static void PaintStatusWindow(HDC hDC, HWND hStatusWnd)
{

	HBITMAP hImeIconBmp, hSymbolBmp;
	HBITMAP hOldBmp;
	HDC hMemDC;

	SetTextColor(hDC, RGB(0x00, 0x00, 0x00));

	SetBkColor(hDC, RGB(0xff, 0xff, 0xff));

	DrawText(hDC, szImeName, lstrlen(szImeName),
			 &sImeG.rcImeName, DT_CENTER | DT_VCENTER | DT_SINGLELINE);

	// load all bitmap
	hSymbolBmp = (HBITMAP) NULL;

	hImeIconBmp = LoadBitmap(hInst, szChinese);

	hSymbolBmp = LoadBitmap(hInst, szSymbol);
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

LRESULT CALLBACK
StatusWndProc(HWND hStatusWnd, u32 uMsg, WPARAM wParam, LPARAM lParam)
{
	//BHJDEBUG("received msg %s", msg_name(uMsg));
	switch (uMsg) {
	case WM_DESTROY:
		DestroyStatusWindow();
		break;
	case WM_SETCURSOR:
		break;
	case WM_MOUSEMOVE:
		break;
	case WM_LBUTTONUP:
		break;

	case WM_IME_NOTIFY:
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

