
/*++

Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

Module Name:

    uisubs.c


++*/


#include <windows.h>
#include <immdev.h>
#include <htmlhelp.h>
#include <imedefs.h>

/**********************************************************************/
/* DrawDragBorder()                                                   */
/**********************************************************************/
void PASCAL DrawDragBorder(HWND hWnd,	// window of IME is dragged
						   LONG lCursorPos,	// the cursor position
						   LONG lCursorOffset)	// the offset form cursor to window org
{
	HDC hDC;
	int cxBorder, cyBorder;
	int x, y;
	RECT rcWnd;

	cxBorder = GetSystemMetrics(SM_CXBORDER);	// width of border
	cyBorder = GetSystemMetrics(SM_CYBORDER);	// height of border

	// get cursor position
	x = (*(LPPOINTS) & lCursorPos).x;
	y = (*(LPPOINTS) & lCursorPos).y;

	// calculate the org by the offset
	x -= (*(LPPOINTS) & lCursorOffset).x;
	y -= (*(LPPOINTS) & lCursorOffset).y;

	// check for the min boundary of the display
	if (x < sImeG.rcWorkArea.left) {
		x = sImeG.rcWorkArea.left;
	}

	if (y < sImeG.rcWorkArea.top) {
		y = sImeG.rcWorkArea.top;
	}
	// check for the max boundary of the display
	GetWindowRect(hWnd, &rcWnd);


	if (x + rcWnd.right - rcWnd.left > sImeG.rcWorkArea.right) {
		x = sImeG.rcWorkArea.right - (rcWnd.right - rcWnd.left);
	}

	if (y + rcWnd.bottom - rcWnd.top > sImeG.rcWorkArea.bottom) {
		y = sImeG.rcWorkArea.bottom - (rcWnd.bottom - rcWnd.top);
	}

	// draw the moving track
	hDC = CreateDC(TEXT("DISPLAY"), NULL, NULL, NULL);

	if (hDC) {
		SelectObject(hDC, GetStockObject(GRAY_BRUSH));

		// ->
		PatBlt(hDC, x, y, rcWnd.right - rcWnd.left - cxBorder, cyBorder,
			   PATINVERT);
		// v
		PatBlt(hDC, x, y + cyBorder, cxBorder, rcWnd.bottom - rcWnd.top -
			   cyBorder, PATINVERT);
		// _>
		PatBlt(hDC, x + cxBorder, y + rcWnd.bottom - rcWnd.top,
			   rcWnd.right - rcWnd.left - cxBorder, -cyBorder, PATINVERT);
		//  v
		PatBlt(hDC, x + rcWnd.right - rcWnd.left, y,
			   -cxBorder, rcWnd.bottom - rcWnd.top - cyBorder, PATINVERT);

		DeleteDC(hDC);
	}

	return;
}

/**********************************************************************/
/* DrawFrameBorder()                                                  */
/**********************************************************************/
void PASCAL DrawFrameBorder(	// border of IME
							   HDC hDC, HWND hWnd)	// window of IME
{
	RECT rcWnd;
	int xWi, yHi;

	GetWindowRect(hWnd, &rcWnd);

	xWi = rcWnd.right - rcWnd.left;
	yHi = rcWnd.bottom - rcWnd.top;

	// 1, ->
	PatBlt(hDC, 0, 0, xWi, 1, WHITENESS);

	// 1, v
	PatBlt(hDC, 0, 0, 1, yHi, WHITENESS);

	// 1, _>
	PatBlt(hDC, 0, yHi, xWi, -1, BLACKNESS);

	// 1,  v
	PatBlt(hDC, xWi, 0, -1, yHi, BLACKNESS);

	xWi -= 2;
	yHi -= 2;

	SelectObject(hDC, GetStockObject(LTGRAY_BRUSH));

	// 2, ->
	PatBlt(hDC, 1, 1, xWi, 1, PATCOPY);

	// 2, v
	PatBlt(hDC, 1, 1, 1, yHi, PATCOPY);

	// 2,  v
	PatBlt(hDC, xWi + 1, 1, -1, yHi, PATCOPY);

	SelectObject(hDC, GetStockObject(GRAY_BRUSH));

	// 2, _>
	PatBlt(hDC, 1, yHi + 1, xWi, -1, PATCOPY);

	xWi -= 2;
	yHi -= 2;

	// 3, ->
	PatBlt(hDC, 2, 2, xWi, 1, PATCOPY);

	// 3, v
	PatBlt(hDC, 2, 2, 1, yHi, PATCOPY);

	// 3,  v
	PatBlt(hDC, xWi + 2, 3, -1, yHi - 1, WHITENESS);

	SelectObject(hDC, GetStockObject(LTGRAY_BRUSH));

	// 3, _>
	PatBlt(hDC, 2, yHi + 2, xWi, -1, PATCOPY);

	SelectObject(hDC, GetStockObject(GRAY_BRUSH));

	xWi -= 2;
	yHi -= 2;

	// 4, ->
	PatBlt(hDC, 3, 3, xWi, 1, PATCOPY);

	// 4, v
	PatBlt(hDC, 3, 3, 1, yHi, PATCOPY);

	SelectObject(hDC, GetStockObject(LTGRAY_BRUSH));

	// 4,  v
	PatBlt(hDC, xWi + 3, 4, -1, yHi - 1, PATCOPY);

	// 4, _>
	PatBlt(hDC, 3, yHi + 3, xWi, -1, WHITENESS);

	return;
}


/**********************************************************************/
/* ContextMenuWndProc()                                               */
/**********************************************************************/
LRESULT CALLBACK
ContextMenuWndProc(HWND hCMenuWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_DESTROY:
		{
			HWND hUIWnd;

			hUIWnd = (HWND) GetWindowLongPtr(hCMenuWnd, CMENU_HUIWND);

			if (hUIWnd) {
				SendMessage(hUIWnd, WM_IME_NOTIFY, IMN_PRIVATE,
							IMN_PRIVATE_CMENUDESTROYED);
			}
		}
		break;
	case WM_USER_DESTROY:
		{
			SendMessage(hCMenuWnd, WM_CLOSE, 0, 0);
			DestroyWindow(hCMenuWnd);
		}
		break;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDM_PROP:
			{
				HIMC hIMC;
				LPINPUTCONTEXT lpIMC;
				LPPRIVCONTEXT lpImcP;
				int UI_MODE;
				HWND hUIWnd;
				RECT rcWorkArea;

				hUIWnd = (HWND) GetWindowLongPtr(hCMenuWnd, CMENU_HUIWND);

				if (!hUIWnd) {
					return (0L);
				}

				rcWorkArea = sImeG.rcWorkArea;

				hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
				if (!hIMC) {
					return (0L);
				}

				lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
				if (!lpIMC) {
					return (0L);
				}

				lpImcP = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate);
				if (!lpImcP) {
					return (0L);
				}

				ImeConfigure(GetKeyboardLayout(0), lpIMC->hWnd,
							 IME_CONFIG_GENERAL, NULL);


				lpImcP->iImeState = CST_INIT;
				CompCancel(hIMC, lpIMC);

				// change compwnd size

				// init fields of hIMC
				lpIMC->fOpen = TRUE;

				if (!(lpIMC->fdwInit & INIT_CONVERSION)) {
					lpIMC->fdwConversion = IME_CMODE_NATIVE;
					lpIMC->fdwInit |= INIT_CONVERSION;
				}

				lpImcP->fdwImeMsg =
					lpImcP->fdwImeMsg | MSG_IMN_DESTROYCAND;
				GenerateMessage(hIMC, lpIMC, lpImcP);

				// set cand window data
				UI_MODE = BOX_UI;
				InitCandUIData(GetSystemMetrics(SM_CXBORDER),
							   GetSystemMetrics(SM_CYBORDER), UI_MODE);

				ImmUnlockIMCC(lpIMC->hPrivate);
				ImmUnlockIMC(hIMC);
				break;
			}
			//case IDM_HLP:
		case IDM_OPTGUD:
			{
				TCHAR szOPTGUDHlpName[MAX_PATH];

				szOPTGUDHlpName[0] = 0;
				GetWindowsDirectory((LPTSTR) szOPTGUDHlpName, MAX_PATH);
				lstrcat((LPTSTR) szOPTGUDHlpName,
						TEXT("\\HELP\\WINIME.CHM"));
				HtmlHelp(hCMenuWnd, szOPTGUDHlpName, HH_DISPLAY_TOPIC, 0L);
			}
			break;
		case IDM_IMEGUD:
			{
				TCHAR szIMEGUDHlpName[MAX_PATH];

				szIMEGUDHlpName[0] = TEXT('\0');
				GetWindowsDirectory((LPTSTR) szIMEGUDHlpName, MAX_PATH);
				lstrcat((LPTSTR) szIMEGUDHlpName, TEXT("\\HELP\\"));
				//COMBO_IME has only one IME help file
				lstrcat((LPTSTR) szIMEGUDHlpName, TEXT("WINGB.CHM"));
				HtmlHelp(hCMenuWnd, szIMEGUDHlpName, HH_DISPLAY_TOPIC, 0L);
			}
			break;
		case IDM_VER:
			{
				HIMC hIMC;
				LPINPUTCONTEXT lpIMC;
				HWND hUIWnd;

				hUIWnd = (HWND) GetWindowLongPtr(hCMenuWnd, CMENU_HUIWND);

				if (!hUIWnd) {
					return (0L);
				}

				hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);

				if (!hIMC) {
					return (0L);
				}


				lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
				if (!lpIMC) {
					return (0L);
				}

				DialogBox(hInst, TEXT("IMEVER"), (HWND) lpIMC->hWnd,
						  (DLGPROC) ImeVerDlgProc);

				ImmUnlockIMC(hIMC);
				break;
			}

		}

		break;

	case WM_CLOSE:
		{
			HMENU hMenu;

			GetMenu(hCMenuWnd);

			hMenu = (HMENU) GetWindowLongPtr(hCMenuWnd, CMENU_MENU);
			if (hMenu) {
				SetWindowLongPtr(hCMenuWnd, CMENU_MENU, (LONG_PTR) NULL);
				DestroyMenu(hMenu);
			}
		}
		return DefWindowProc(hCMenuWnd, uMsg, wParam, lParam);
	default:
		return DefWindowProc(hCMenuWnd, uMsg, wParam, lParam);
	}

	return (0L);
}


/**********************************************************************/
/* ContextMenu()                                                      */
/**********************************************************************/
void PASCAL ContextMenu(HWND hStatusWnd, int x, int y)
{
	HWND hUIWnd;
	HWND hCMenuWnd;
	HGLOBAL hUIPrivate;
	LPUIPRIV lpUIPrivate;
	HIMC hIMC;
	LPINPUTCONTEXT lpIMC;
	HMENU hMenu, hCMenu;
	RECT rcStatusWnd;

	hUIWnd = GetWindow(hStatusWnd, GW_OWNER);
	if (!hUIWnd) {
		return;
	}
	GetWindowRect(hStatusWnd, &rcStatusWnd);

	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	if (!hIMC) {
		return;
	}

	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
	if (!lpIMC) {
		return;
	}

	hUIPrivate = (HGLOBAL) GetWindowLongPtr(hUIWnd, IMMGWLP_PRIVATE);
	if (!hUIPrivate) {
		goto ContextMenuUnlockIMC;
	}

	lpUIPrivate = (LPUIPRIV) GlobalLock(hUIPrivate);
	if (!lpUIPrivate) {
		goto ContextMenuUnlockIMC;
	}

	if (!lpUIPrivate->hCMenuWnd) {
		// this is important to assign owner window, otherwise the focus
		// will be gone

		// When UI terminate, it need to destroy this window
		lpUIPrivate->hCMenuWnd = CreateWindowEx(CS_HREDRAW | CS_VREDRAW,
												szCMenuClassName,
												TEXT("Context Menu"),
												WS_POPUP | WS_DISABLED, 0,
												0, 0, 0, lpIMC->hWnd,
												(HMENU) NULL, hInst, NULL);

	}

	hCMenuWnd = lpUIPrivate->hCMenuWnd;

	// Unlock before we call into TrackPopupMenu().
	GlobalUnlock(hUIPrivate);

	if (!hCMenuWnd) {
		goto ContextMenuUnlockIMC;
	}

	hMenu = LoadMenu(hInst, TEXT("PROPMENU"));
	hCMenu = GetSubMenu(hMenu, 0);

	// Disable some of menu items.

	if (lpImeL->fWinLogon == TRUE) {
		// In Logon Mode, we don't want to show help and configuration dialog

		EnableMenuItem(hCMenu, 0, MF_BYPOSITION | MF_GRAYED);
		EnableMenuItem(hCMenu, IDM_PROP, MF_BYCOMMAND | MF_GRAYED);
	}

	SetWindowLongPtr(hCMenuWnd, CMENU_HUIWND, (LONG_PTR) hUIWnd);
	SetWindowLongPtr(hCMenuWnd, CMENU_MENU, (LONG_PTR) hMenu);

	TrackPopupMenu(hCMenu, TPM_LEFTBUTTON,
				   rcStatusWnd.left, rcStatusWnd.top, 0, hCMenuWnd, NULL);

	hMenu = (HMENU) GetWindowLongPtr(hCMenuWnd, CMENU_MENU);
	if (hMenu) {
		SetWindowLongPtr(hCMenuWnd, CMENU_MENU, (LONG_PTR) NULL);
		DestroyMenu(hMenu);
	}

  ContextMenuUnlockIMC:
	ImmUnlockIMC(hIMC);

	return;
}
