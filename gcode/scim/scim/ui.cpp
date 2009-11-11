
/*++

Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

Module Name:

    ui.c


++*/


#include <windows.h>
#include <immdev.h>
#include <imedefs.h>
#include <regstr.h>



/**********************************************************************/
/* CMenuDestryed()                                                    */
/**********************************************************************/
void PASCAL CMenuDestroyed(		// context menu window
							  // already destroyed
							  HWND hUIWnd)
{
	hCMenuWnd = NULL;
}

void PASCAL CreateUIWindow(HWND hUIWnd)
{

	SetWindowPos(hUIWnd, NULL, 0, 0, 0, 0, SWP_NOACTIVATE | SWP_NOZORDER);

	ShowWindow(hUIWnd, SW_SHOWNOACTIVATE);

	return;
}

void PASCAL DestroyUIWindow(	// destroy composition window
							   HWND hUIWnd)
{

	//destroy ContextMenuWnd
	if (hCMenuWnd) {
		SetWindowLongPtr(hCMenuWnd, CMENU_HUIWND,
						 (LONG_PTR) 0);
		PostMessage(hCMenuWnd, WM_USER_DESTROY, 0, 0);
	}

	// composition window need to be destroyed
	if (hCompWnd) {
		DestroyWindow(hCompWnd);
		hCompWnd = NULL;
	}
	// candidate window need to be destroyed
	if (hCandWnd) {
		DestroyWindow(hCandWnd);
		hCandWnd = NULL;
	}
	// status window need to be destroyed
	if (hStatusWnd) {
		DestroyWindow(hStatusWnd);
		hStatusWnd = NULL;
	}

	return;
}

/**********************************************************************/
/* StatusWndMsg()                                                     */
/**********************************************************************/
void PASCAL StatusWndMsg(		// set the show hide state and
							HWND hUIWnd, BOOL fOn)
{
	HIMC hIMC;


	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	if (!hIMC) {
		return;
	}

	if (fOn) {

		if (!hStatusWnd) {
			OpenStatus(hUIWnd);
		}
	} 

	if (!hStatusWnd) {
		return;
	}

	if (!fOn) {
	} else if (hIMC) {
		ShowStatus(hUIWnd, SW_SHOWNOACTIVATE);
	} else {
		ShowStatus(hUIWnd, SW_HIDE);
	}

	return;
}

/**********************************************************************/
/* ShowUI()                                                           */
/**********************************************************************/
void PASCAL ShowUI(				// show the sub windows
					  HWND hUIWnd, int nShowCmd)
{
	BHJDEBUG(" ShowUI called");
	HIMC hIMC;
	LPINPUTCONTEXT lpIMC;
	LPPRIVCONTEXT imcPrivPtr;

	if (nShowCmd == SW_HIDE) {
	} else if (!(hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC))) {
		nShowCmd = SW_HIDE;
	} else if (!(lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC))) {
		nShowCmd = SW_HIDE;
	} else if (!(imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate))) {
		ImmUnlockIMC(hIMC);
		nShowCmd = SW_HIDE;
	} 

	if (nShowCmd == SW_HIDE) {
		ShowStatus(hUIWnd, nShowCmd);
		ShowComp(hUIWnd, nShowCmd);
		ShowCand(hUIWnd, nShowCmd);
		return;
	}

	if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_START) {
		if (hCompWnd) {

			RedrawWindow(hCompWnd, NULL, NULL,
						 RDW_FRAME | RDW_INVALIDATE | RDW_ERASE);
			SendMessage(hCompWnd, WM_IME_NOTIFY,
						IMN_SETCOMPOSITIONWINDOW, 0);

		} else {
			StartComp(hUIWnd);
		}
	} else {
		ShowComp(hUIWnd, SW_HIDE);
	}

	if (imcPrivPtr->fdwImeMsg & MSG_ALREADY_OPEN) {
		BHJDEBUG(" RedrawWindow called");
		if (hCandWnd) {
			RedrawWindow(hCandWnd, NULL, NULL,
						 RDW_FRAME | RDW_INVALIDATE | RDW_ERASE);
			
			SendMessage(hCandWnd, WM_IME_NOTIFY,
						IMN_SETCANDIDATEPOS, 0x0001);
		} else {
			OpenCand(hUIWnd);
		}
	} else {
		BHJDEBUG(" RedrawWindow not called");
		ShowCand(hUIWnd, SW_HIDE);
	}


	if (!hStatusWnd) {
		OpenStatus(hUIWnd);
	}
	RedrawWindow(hStatusWnd, NULL, NULL,
				 RDW_FRAME | RDW_INVALIDATE | RDW_ERASE);

	SendMessage(hStatusWnd, WM_IME_NOTIFY,
				IMN_SETSTATUSWINDOWPOS, 0);
	ShowStatus(hUIWnd, nShowCmd);

	ImmUnlockIMCC(lpIMC->hPrivate);
	ImmUnlockIMC(hIMC);

	return;
}

/**********************************************************************/
/* ShowGuideLine                                                      */
/**********************************************************************/
void PASCAL ShowGuideLine(HWND hUIWnd)
{
	HIMC hIMC;
	LPINPUTCONTEXT lpIMC;
	LPGUIDELINE lpGuideLine;

	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	if (!hIMC) {
		return;
	}

	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
	if (!lpIMC) {
		return;
	}

	lpGuideLine = (LPGUIDELINE) ImmLockIMCC(lpIMC->hGuideLine);

	if (!lpGuideLine) {
	} else if (lpGuideLine->dwLevel == GL_LEVEL_ERROR) {
		MessageBeep((UINT) - 1);
		MessageBeep((UINT) - 1);
	} else if (lpGuideLine->dwLevel == GL_LEVEL_WARNING) {
		MessageBeep((UINT) - 1);
	} else {
	}

	ImmUnlockIMCC(lpIMC->hGuideLine);
	ImmUnlockIMC(hIMC);

	return;
}

/**********************************************************************/
/* UpdateStatusWindow()                                               */
/* Return Value:                                                      */
/*     none                                                             */
/**********************************************************************/
BOOL UpdateStatusWindow(HWND hUIWnd)
{
	HWND hStatusWnd;

	if (!(hStatusWnd = GetStatusWnd(hUIWnd))) {
		return FALSE;
	}

	InvalidateRect(hStatusWnd, &(sImeG.rcStatusText), TRUE);
	UpdateWindow(hStatusWnd);

	return (TRUE);
}

void PASCAL NotifyUI(HWND hUIWnd, WPARAM wParam, LPARAM lParam)
{
	HWND hStatusWnd;

	switch (wParam) {
	case IMN_OPENSTATUSWINDOW:
		//PostStatus(hUIWnd, TRUE);
		StatusWndMsg(hUIWnd, TRUE);
		break;
	case IMN_CLOSESTATUSWINDOW:
		//PostStatus(hUIWnd, FALSE);
		StatusWndMsg(hUIWnd, FALSE);
		break;
	case IMN_OPENCANDIDATE:
		if (lParam & 0x00000001) {
			OpenCand(hUIWnd);
		}
		break;
	case IMN_CHANGECANDIDATE:
		if (lParam & 0x00000001) {
			HDC hDC;
			HWND hCandWnd;

			hCandWnd = GetCandWnd(hUIWnd);
			if (!hCandWnd) {
				return;
			}

			hDC = GetDC(hCandWnd);
			PaintCandWindow(hCandWnd, hDC);
			ReleaseDC(hCandWnd, hDC);
		}
		break;
	case IMN_CLOSECANDIDATE:
		if (lParam & 0x00000001) {
			CloseCand(hUIWnd);
		}
		break;
	case IMN_SETSENTENCEMODE:
		break;
	case IMN_SETOPENSTATUS:
	case IMN_SETCONVERSIONMODE:
		hStatusWnd = GetStatusWnd(hUIWnd);
		if (!hStatusWnd) {
			return;
		}

		{
			RECT rcRect;

			rcRect = sImeG.rcStatusText;

			// off by 1
			rcRect.right += 1;
			rcRect.bottom += 1;

			RedrawWindow(hStatusWnd, &rcRect, NULL, RDW_INVALIDATE);
		}
		break;
	case IMN_SETCOMPOSITIONFONT:
		// we are not going to change font, but an IME can do this if it want
		break;
	case IMN_SETCOMPOSITIONWINDOW:
		{
			HWND hCompWnd;

			hCompWnd = GetCompWnd(hUIWnd);
			if (!hCompWnd) {
				return;
			}

			PostMessage(hCompWnd, WM_IME_NOTIFY, wParam, lParam);
		}
		break;
	case IMN_SETCANDIDATEPOS:
		{
			HWND hCandWnd;

			hCandWnd = GetCandWnd(hUIWnd);
			if (!hCandWnd) {
				return;
			}

			PostMessage(hCandWnd, WM_IME_NOTIFY, wParam, lParam);
		}
		break;
	case IMN_SETSTATUSWINDOWPOS:
		hStatusWnd = GetStatusWnd(hUIWnd);
		if (hStatusWnd) {
			PostMessage(hStatusWnd, WM_IME_NOTIFY, wParam, lParam);
		} else {
		}
		break;
	case IMN_GUIDELINE:
		ShowGuideLine(hUIWnd);

		break;
	case IMN_PRIVATE:
		switch (lParam) {
		case IMN_PRIVATE_UPDATE_STATUS:
			UpdateStatusWindow(hUIWnd);
			break;
		case IMN_PRIVATE_DESTROYCANDWIN:
			SendMessage(GetCandWnd(hUIWnd), WM_DESTROY, (WPARAM) 0,
						(LPARAM) 0);
			break;
		case IMN_PRIVATE_CMENUDESTROYED:
			CMenuDestroyed(hUIWnd);
			break;
		}
		break;
	default:
		break;
	}

	return;
}

/**********************************************************************/
/* SetContext()                                                       */
/**********************************************************************/
void PASCAL SetContext(			// the context activated/deactivated
						  HWND hUIWnd, BOOL fOn, LPARAM lShowUI)
{
	EnterLeaveDebug(); 
	HIMC hIMC;
	LPINPUTCONTEXT lpIMC;
	LPPRIVCONTEXT imcPrivPtr;
	RECT rcWorkArea;

	rcWorkArea = sImeG.rcWorkArea;


	hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);

	if (!hIMC) {
		return;
	}
	// get lpIMC
	lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
	if (!lpIMC) {
		return;
	}

	if (fOn) {

		ShowComp(hUIWnd, SW_SHOW);
		ShowCand(hUIWnd, SW_SHOW);

		if (lpIMC->cfCandForm[0].dwIndex != 0) {
			lpIMC->cfCandForm[0].dwStyle = CFS_DEFAULT;
		}


		imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate);
		if (!imcPrivPtr) {
			ImmUnlockIMC(hIMC);
			return;
		}


		if (SaTC_Trace == 0) {
			int UI_MODE;

			imcPrivPtr->iImeState = CST_INIT;
			CompCancel(hIMC, lpIMC);

			// init fields of hPrivate
			imcPrivPtr->fdwImeMsg = (DWORD) 0;
			imcPrivPtr->dwCompChar = (DWORD) 0;
			imcPrivPtr->fdwGcsFlag = (DWORD) 0;
			imcPrivPtr->uSYHFlg = 0x00000000;
			imcPrivPtr->uDYHFlg = 0x00000000;

			// change compwnd size

			// init fields of hIMC
			lpIMC->fOpen = TRUE;

			SendMessage(GetCandWnd(hUIWnd), WM_DESTROY, (WPARAM) 0,
						(LPARAM) 0);

			// set cand window data
			UI_MODE = BOX_UI;
			InitCandUIData(GetSystemMetrics(SM_CXBORDER),
						   GetSystemMetrics(SM_CYBORDER), UI_MODE);
		}

		SaTC_Trace = 1;

		// init Caps
		{
			BYTE lpbKeyState[256];
			DWORD fdwConversion;

			if (!GetKeyboardState(lpbKeyState))
				lpbKeyState[VK_CAPITAL] = 0;

			if (lpbKeyState[VK_CAPITAL] & 0x01) {
				// 10.11 add
				uCaps = 1;
				// change to alphanumeric mode
				fdwConversion =
					lpIMC->
					fdwConversion & ~(0 | IME_CMODE_NATIVE
									  | IME_CMODE_EUDC);
			} else {
				// change to native mode
				if (uCaps == 1) {
					fdwConversion =
						(lpIMC->
						 fdwConversion | IME_CMODE_NATIVE) &
						~(0 | IME_CMODE_EUDC);
				} else {
					fdwConversion = lpIMC->fdwConversion;
				}
				uCaps = 0;
			}
			ImmSetConversionStatus(hIMC, fdwConversion,
								   lpIMC->fdwSentence);
		}

		if ((lpIMC->cfCompForm.dwStyle & CFS_FORCE_POSITION)) {
			POINT ptNew;		// new position of UI
			POINT ptSTWPos;

			ImmGetStatusWindowPos(hIMC, (LPPOINT) & ptSTWPos);

			ptNew.x = ptSTWPos.x + sImeG.xStatusWi + UI_MARGIN;
			if ((ptSTWPos.x + sImeG.xStatusWi + sImeG.xCandWi +
				 lpImeL->xCompWi + 2 * UI_MARGIN) >= rcWorkArea.right) {
				ptNew.x = ptSTWPos.x - lpImeL->xCompWi - UI_MARGIN;
			}
			ptNew.x += lpImeL->cxCompBorder;
			ptNew.y = ptSTWPos.y + lpImeL->cyCompBorder;
			lpIMC->cfCompForm.ptCurrentPos = ptNew;

			ScreenToClient(lpIMC->hWnd, &lpIMC->cfCompForm.ptCurrentPos);
			lpIMC->cfCompForm.dwStyle = CFS_DEFAULT;
		}
	} 

	UIPaint(hUIWnd);

	ImmUnlockIMC(hIMC);
	return;
}


/**********************************************************************/
/* GetCompWindow()                                                    */
/**********************************************************************/
LRESULT PASCAL GetCompWindow(HWND hUIWnd, LPCOMPOSITIONFORM lpCompForm)
{
	HWND hCompWnd;
	RECT rcCompWnd;

	hCompWnd = GetCompWnd(hUIWnd);

	if (!hCompWnd) {
		return (1L);
	}

	if (!GetWindowRect(hCompWnd, &rcCompWnd)) {
		return (1L);
	}

	lpCompForm->dwStyle = CFS_POINT | CFS_RECT;
	lpCompForm->ptCurrentPos = *(LPPOINT) & rcCompWnd;
	lpCompForm->rcArea = rcCompWnd;

	return (0L);
}

/**********************************************************************/
/* SelectIME()                                                        */
/**********************************************************************/
void PASCAL SelectIME(			// switch IMEs
						 HWND hUIWnd, BOOL fSelect)
{
	if (!fSelect) {
		ShowUI(hUIWnd, SW_HIDE);
	} else {

		ShowUI(hUIWnd, SW_SHOWNOACTIVATE);

	}

	return;
}

LRESULT PASCAL UIPaint(HWND hUIWnd)
{
	PAINTSTRUCT ps;
	MSG sMsg;

	// for safety
	BeginPaint(hUIWnd, &ps);
	EndPaint(hUIWnd, &ps);

	// some application will not remove the WM_PAINT messages
	PeekMessage(&sMsg, hUIWnd, WM_PAINT, WM_PAINT, PM_REMOVE | PM_NOYIELD);
	ShowUI(hUIWnd, SW_SHOWNOACTIVATE);


	return (0L);
}

/**********************************************************************/
/* UIWndProc()                                                        */
/**********************************************************************/
LRESULT CALLBACK
UIWndProc(HWND hUIWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	BHJDEBUG("received msg %s", msg_name(uMsg));
	switch (uMsg) {
	case WM_CREATE:
		CreateUIWindow(hUIWnd);
		break;
	case WM_DESTROY:
		DestroyUIWindow(hUIWnd);
		break;
	case WM_IME_STARTCOMPOSITION:
		// you can create a window as the composition window here
		StartComp(hUIWnd);
		break;
	case WM_IME_COMPOSITION:
		if (lParam & GCS_RESULTSTR) {
			MoveDefaultCompPosition(hUIWnd);
		} else {
		}

		{
			HWND hCompWnd;

			hCompWnd = GetCompWnd(hUIWnd);

			if (hCompWnd) {
				RECT rcRect;

				rcRect = lpImeL->rcCompText;
				// off by 1
				rcRect.right += 1;
				rcRect.bottom += 1;

				RedrawWindow(hCompWnd, &rcRect, NULL, RDW_INVALIDATE);
			}
		}
		break;
	case WM_IME_ENDCOMPOSITION:
		// you can destroy the composition window here
		EndComp(hUIWnd);
		break;
	case WM_IME_NOTIFY:
		NotifyUI(hUIWnd, wParam, lParam);
		break;
	case WM_IME_SETCONTEXT:
		SetContext(hUIWnd, (BOOL) wParam, lParam);

		if (wParam && GetWindowLongPtr(hUIWnd, IMMGWLP_IMC))
			SetWindowPos(hUIWnd, NULL, 0, 0, 0, 0,
						 SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOMOVE);

		break;
	case WM_IME_CONTROL:
		switch (wParam) {
		case IMC_GETCANDIDATEPOS:
			return (1L);		// not implemented yet
		case IMC_GETCOMPOSITIONFONT:
			return (1L);		// not implemented yet
		case IMC_GETCOMPOSITIONWINDOW:
			return GetCompWindow(hUIWnd, (LPCOMPOSITIONFORM) lParam);
		case IMC_GETSTATUSWINDOWPOS:
			{
				HWND hStatusWnd;
				RECT rcStatusWnd;
				LPARAM lParam;

				hStatusWnd = GetStatusWnd(hUIWnd);
				if (!hStatusWnd) {
					return (0L);	// fail, return (0, 0)?
				}

				if (!GetWindowRect(hStatusWnd, &rcStatusWnd)) {
					return (0L);	// fail, return (0, 0)?
				}

				lParam = MAKELRESULT(rcStatusWnd.left, rcStatusWnd.top);

				return (lParam);
			}
			return (0L);
		case IMC_SETSTATUSWINDOWPOS:
			{
				HIMC hIMC;
				LPINPUTCONTEXT lpIMC;
				LPPRIVCONTEXT imcPrivPtr;
				//COMPOSITIONFORM CompForm;
				POINT ptPos;
				RECT rcWorkArea;

				rcWorkArea = sImeG.rcWorkArea;

				ptPos.x = ((LPPOINTS) & lParam)->x;
				ptPos.y = ((LPPOINTS) & lParam)->y;

				hIMC = (HIMC) GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
				if (!hIMC) {
					return (1L);
				}

				if (!ImmSetStatusWindowPos(hIMC, &ptPos)) {
					return (1L);
				}
				// set comp window position when TraceCuer
				lpIMC = (LPINPUTCONTEXT) ImmLockIMC(hIMC);
				if (!lpIMC) {
					return (1L);
				}

				imcPrivPtr = (LPPRIVCONTEXT) ImmLockIMCC(lpIMC->hPrivate);
				if (!imcPrivPtr) {
					return (1L);
				}

				ImmUnlockIMCC(lpIMC->hPrivate);
				ImmUnlockIMC(hIMC);

				return (0L);
			}
			return (1L);
		default:
			return (1L);
		}
		break;
	case WM_IME_COMPOSITIONFULL:
		return (0L);
	case WM_IME_SELECT:

		SetContext(hUIWnd, (BOOL) wParam, 0);
		return (0L);
	case WM_MOUSEACTIVATE:
		return (MA_NOACTIVATE);
	default:
		BHJDEBUG(" msg %s not handled", msg_name(uMsg));
		return DefWindowProc(hUIWnd, uMsg, wParam, lParam);
	}
	return (0L);
}

void DrawConvexRect(HDC hDC, int x1, int y1, int x2, int y2)
{
	HPEN hPen, hOldPen;

	SelectObject(hDC, GetStockObject(LTGRAY_BRUSH));
	SelectObject(hDC, GetStockObject(WHITE_PEN));
	MoveToEx(hDC, x1, y1, NULL);
	LineTo(hDC, x2, y1);
	MoveToEx(hDC, x1, y1, NULL);
	LineTo(hDC, x1, y2);
	hPen = CreatePen(PS_SOLID, 1, RGB(128, 128, 128));

	if (hPen) {
		hOldPen = SelectObject(hDC, hPen);
		MoveToEx(hDC, x2 - 1, y2 - 1, NULL);
		LineTo(hDC, x2 - 1, y1);
		MoveToEx(hDC, x2 - 1, y2 - 1, NULL);
		LineTo(hDC, x1, y2 - 1);
		SelectObject(hDC, hOldPen);
		DeleteObject(hPen);
	}
}

void DrawConvexRectP(HDC hDC, int x1, int y1, int x2, int y2)
{
	HPEN hPen, hOldPen;

	SelectObject(hDC, GetStockObject(LTGRAY_BRUSH));
	SelectObject(hDC, GetStockObject(WHITE_PEN));
	MoveToEx(hDC, x1, y1, NULL);
	LineTo(hDC, x2 - 1, y1);
	MoveToEx(hDC, x1, y1, NULL);
	LineTo(hDC, x1, y2 - 1);
	hPen = CreatePen(PS_SOLID, 1, RGB(0, 0, 0));

	if (hPen) {
		hOldPen = SelectObject(hDC, hPen);
		MoveToEx(hDC, x2 - 1, y2 - 1, NULL);
		LineTo(hDC, x2 - 1, y1);
		MoveToEx(hDC, x2 - 1, y2 - 1, NULL);
		LineTo(hDC, x1, y2 - 1);
		SelectObject(hDC, hOldPen);
		DeleteObject(hPen);
	}
}

void DrawConcaveRect(HDC hDC, int x1, int y1, int x2, int y2)
{
	HPEN hLtPen = CreatePen(PS_SOLID, 1, 0x00808080);

	if (hLtPen != NULL) {
		HPEN OldPen = SelectObject(hDC, hLtPen);
		MoveToEx(hDC, x1, y1, NULL);
		LineTo(hDC, x2, y1);
		MoveToEx(hDC, x1, y1, NULL);
		LineTo(hDC, x1, y2);
		SelectObject(hDC, GetStockObject(WHITE_PEN));
		MoveToEx(hDC, x2, y2, NULL);
		LineTo(hDC, x2, y1 - 1);
		MoveToEx(hDC, x2, y2, NULL);
		LineTo(hDC, x1 - 1, y2);
		SelectObject(hDC, OldPen);
		DeleteObject(hLtPen);
	}
}

struct {
	const char* name;
	UINT msg;
} msg_name_map [] = {
	{"WM_NULL", WM_NULL},
	{"WM_CREATE", WM_CREATE},
	{"WM_DESTROY", WM_DESTROY},
	{"WM_MOVE", WM_MOVE},
	{"WM_SIZE", WM_SIZE},
	{"WM_ACTIVATE", WM_ACTIVATE},
	{"WM_SETFOCUS", WM_SETFOCUS},
	{"WM_KILLFOCUS", WM_KILLFOCUS},
	{"WM_ENABLE", WM_ENABLE},
	{"WM_SETREDRAW", WM_SETREDRAW},
	{"WM_SETTEXT", WM_SETTEXT},
	{"WM_GETTEXT", WM_GETTEXT},
	{"WM_GETTEXTLENGTH", WM_GETTEXTLENGTH},
	{"WM_PAINT", WM_PAINT},
	{"WM_CLOSE", WM_CLOSE},
	{"WM_QUERYENDSESSION", WM_QUERYENDSESSION},
	{"WM_QUERYOPEN", WM_QUERYOPEN},
	{"WM_ENDSESSION", WM_ENDSESSION},
	{"WM_QUIT", WM_QUIT},
	{"WM_ERASEBKGND", WM_ERASEBKGND},
	{"WM_SYSCOLORCHANGE", WM_SYSCOLORCHANGE},
	{"WM_SHOWWINDOW", WM_SHOWWINDOW},
	{"WM_WININICHANGE", WM_WININICHANGE},
	{"WM_SETTINGCHANGE", WM_SETTINGCHANGE},
	{"WM_DEVMODECHANGE", WM_DEVMODECHANGE},
	{"WM_ACTIVATEAPP", WM_ACTIVATEAPP},
	{"WM_FONTCHANGE", WM_FONTCHANGE},
	{"WM_TIMECHANGE", WM_TIMECHANGE},
	{"WM_CANCELMODE", WM_CANCELMODE},
	{"WM_SETCURSOR", WM_SETCURSOR},
	{"WM_MOUSEACTIVATE", WM_MOUSEACTIVATE},
	{"WM_CHILDACTIVATE", WM_CHILDACTIVATE},
	{"WM_QUEUESYNC", WM_QUEUESYNC},
	{"WM_GETMINMAXINFO", WM_GETMINMAXINFO},
	{"WM_PAINTICON", WM_PAINTICON},
	{"WM_ICONERASEBKGND", WM_ICONERASEBKGND},
	{"WM_NEXTDLGCTL", WM_NEXTDLGCTL},
	{"WM_SPOOLERSTATUS", WM_SPOOLERSTATUS},
	{"WM_DRAWITEM", WM_DRAWITEM},
	{"WM_MEASUREITEM", WM_MEASUREITEM},
	{"WM_DELETEITEM", WM_DELETEITEM},
	{"WM_VKEYTOITEM", WM_VKEYTOITEM},
	{"WM_CHARTOITEM", WM_CHARTOITEM},
	{"WM_SETFONT", WM_SETFONT},
	{"WM_GETFONT", WM_GETFONT},
	{"WM_SETHOTKEY", WM_SETHOTKEY},
	{"WM_GETHOTKEY", WM_GETHOTKEY},
	{"WM_QUERYDRAGICON", WM_QUERYDRAGICON},
	{"WM_COMPAREITEM", WM_COMPAREITEM},
	{"WM_GETOBJECT", WM_GETOBJECT},
	{"WM_COMPACTING", WM_COMPACTING},
	{"WM_COMMNOTIFY", WM_COMMNOTIFY},
	{"WM_WINDOWPOSCHANGING", WM_WINDOWPOSCHANGING},
	{"WM_WINDOWPOSCHANGED", WM_WINDOWPOSCHANGED},
	{"WM_POWER", WM_POWER},
	{"WM_COPYDATA", WM_COPYDATA},
	{"WM_CANCELJOURNAL", WM_CANCELJOURNAL},
	{"WM_NOTIFY", WM_NOTIFY},
	{"WM_INPUTLANGCHANGEREQUEST", WM_INPUTLANGCHANGEREQUEST},
	{"WM_INPUTLANGCHANGE", WM_INPUTLANGCHANGE},
	{"WM_TCARD", WM_TCARD},
	{"WM_HELP", WM_HELP},
	{"WM_USERCHANGED", WM_USERCHANGED},
	{"WM_NOTIFYFORMAT", WM_NOTIFYFORMAT},
	{"WM_CONTEXTMENU", WM_CONTEXTMENU},
	{"WM_STYLECHANGING", WM_STYLECHANGING},
	{"WM_STYLECHANGED", WM_STYLECHANGED},
	{"WM_DISPLAYCHANGE", WM_DISPLAYCHANGE},
	{"WM_GETICON", WM_GETICON},
	{"WM_SETICON", WM_SETICON},
	{"WM_NCCREATE", WM_NCCREATE},
	{"WM_NCDESTROY", WM_NCDESTROY},
	{"WM_NCCALCSIZE", WM_NCCALCSIZE},
	{"WM_NCHITTEST", WM_NCHITTEST},
	{"WM_NCPAINT", WM_NCPAINT},
	{"WM_NCACTIVATE", WM_NCACTIVATE},
	{"WM_GETDLGCODE", WM_GETDLGCODE},
	{"WM_SYNCPAINT", WM_SYNCPAINT},
	{"WM_NCMOUSEMOVE", WM_NCMOUSEMOVE},
	{"WM_NCLBUTTONDOWN", WM_NCLBUTTONDOWN},
	{"WM_NCLBUTTONUP", WM_NCLBUTTONUP},
	{"WM_NCLBUTTONDBLCLK", WM_NCLBUTTONDBLCLK},
	{"WM_NCRBUTTONDOWN", WM_NCRBUTTONDOWN},
	{"WM_NCRBUTTONUP", WM_NCRBUTTONUP},
	{"WM_NCRBUTTONDBLCLK", WM_NCRBUTTONDBLCLK},
	{"WM_NCMBUTTONDOWN", WM_NCMBUTTONDOWN},
	{"WM_NCMBUTTONUP", WM_NCMBUTTONUP},
	{"WM_NCMBUTTONDBLCLK", WM_NCMBUTTONDBLCLK},
	{"WM_NCXBUTTONDOWN", WM_NCXBUTTONDOWN},
	{"WM_NCXBUTTONUP", WM_NCXBUTTONUP},
	{"WM_NCXBUTTONDBLCLK", WM_NCXBUTTONDBLCLK},
	{"WM_INPUT", WM_INPUT},
	{"WM_KEYFIRST", WM_KEYFIRST},
	{"WM_KEYDOWN", WM_KEYDOWN},
	{"WM_KEYUP", WM_KEYUP},
	{"WM_CHAR", WM_CHAR},
	{"WM_DEADCHAR", WM_DEADCHAR},
	{"WM_SYSKEYDOWN", WM_SYSKEYDOWN},
	{"WM_SYSKEYUP", WM_SYSKEYUP},
	{"WM_SYSCHAR", WM_SYSCHAR},
	{"WM_SYSDEADCHAR", WM_SYSDEADCHAR},
	{"WM_UNICHAR", WM_UNICHAR},
	{"WM_KEYLAST", WM_KEYLAST},
	{"WM_KEYLAST", WM_KEYLAST},
	{"WM_IME_STARTCOMPOSITION", WM_IME_STARTCOMPOSITION},
	{"WM_IME_ENDCOMPOSITION", WM_IME_ENDCOMPOSITION},
	{"WM_IME_COMPOSITION", WM_IME_COMPOSITION},
	{"WM_IME_KEYLAST", WM_IME_KEYLAST},
	{"WM_INITDIALOG", WM_INITDIALOG},
	{"WM_COMMAND", WM_COMMAND},
	{"WM_SYSCOMMAND", WM_SYSCOMMAND},
	{"WM_TIMER", WM_TIMER},
	{"WM_HSCROLL", WM_HSCROLL},
	{"WM_VSCROLL", WM_VSCROLL},
	{"WM_INITMENU", WM_INITMENU},
	{"WM_INITMENUPOPUP", WM_INITMENUPOPUP},
	{"WM_MENUSELECT", WM_MENUSELECT},
	{"WM_MENUCHAR", WM_MENUCHAR},
	{"WM_ENTERIDLE", WM_ENTERIDLE},
	{"WM_MENURBUTTONUP", WM_MENURBUTTONUP},
	{"WM_MENUDRAG", WM_MENUDRAG},
	{"WM_MENUGETOBJECT", WM_MENUGETOBJECT},
	{"WM_UNINITMENUPOPUP", WM_UNINITMENUPOPUP},
	{"WM_MENUCOMMAND", WM_MENUCOMMAND},
	{"WM_CHANGEUISTATE", WM_CHANGEUISTATE},
	{"WM_UPDATEUISTATE", WM_UPDATEUISTATE},
	{"WM_QUERYUISTATE", WM_QUERYUISTATE},
	{"WM_CTLCOLORMSGBOX", WM_CTLCOLORMSGBOX},
	{"WM_CTLCOLOREDIT", WM_CTLCOLOREDIT},
	{"WM_CTLCOLORLISTBOX", WM_CTLCOLORLISTBOX},
	{"WM_CTLCOLORBTN", WM_CTLCOLORBTN},
	{"WM_CTLCOLORDLG", WM_CTLCOLORDLG},
	{"WM_CTLCOLORSCROLLBAR", WM_CTLCOLORSCROLLBAR},
	{"WM_CTLCOLORSTATIC", WM_CTLCOLORSTATIC},
	{"WM_MOUSEFIRST", WM_MOUSEFIRST},
	{"WM_MOUSEMOVE", WM_MOUSEMOVE},
	{"WM_LBUTTONDOWN", WM_LBUTTONDOWN},
	{"WM_LBUTTONUP", WM_LBUTTONUP},
	{"WM_LBUTTONDBLCLK", WM_LBUTTONDBLCLK},
	{"WM_RBUTTONDOWN", WM_RBUTTONDOWN},
	{"WM_RBUTTONUP", WM_RBUTTONUP},
	{"WM_RBUTTONDBLCLK", WM_RBUTTONDBLCLK},
	{"WM_MBUTTONDOWN", WM_MBUTTONDOWN},
	{"WM_MBUTTONUP", WM_MBUTTONUP},
	{"WM_MBUTTONDBLCLK", WM_MBUTTONDBLCLK},
	{"WM_MOUSEWHEEL", WM_MOUSEWHEEL},
	{"WM_XBUTTONDOWN", WM_XBUTTONDOWN},
	{"WM_XBUTTONUP", WM_XBUTTONUP},
	{"WM_XBUTTONDBLCLK", WM_XBUTTONDBLCLK},
	{"WM_MOUSELAST", WM_MOUSELAST},
	{"WM_MOUSELAST", WM_MOUSELAST},
	{"WM_MOUSELAST", WM_MOUSELAST},
	{"WM_PARENTNOTIFY", WM_PARENTNOTIFY},
	{"WM_ENTERMENULOOP", WM_ENTERMENULOOP},
	{"WM_EXITMENULOOP", WM_EXITMENULOOP},
	{"WM_NEXTMENU", WM_NEXTMENU},
	{"WM_SIZING", WM_SIZING},
	{"WM_CAPTURECHANGED", WM_CAPTURECHANGED},
	{"WM_MOVING", WM_MOVING},
	{"WM_POWERBROADCAST", WM_POWERBROADCAST},
	{"WM_DEVICECHANGE", WM_DEVICECHANGE},
	{"WM_MDICREATE", WM_MDICREATE},
	{"WM_MDIDESTROY", WM_MDIDESTROY},
	{"WM_MDIACTIVATE", WM_MDIACTIVATE},
	{"WM_MDIRESTORE", WM_MDIRESTORE},
	{"WM_MDINEXT", WM_MDINEXT},
	{"WM_MDIMAXIMIZE", WM_MDIMAXIMIZE},
	{"WM_MDITILE", WM_MDITILE},
	{"WM_MDICASCADE", WM_MDICASCADE},
	{"WM_MDIICONARRANGE", WM_MDIICONARRANGE},
	{"WM_MDIGETACTIVE", WM_MDIGETACTIVE},
	{"WM_MDISETMENU", WM_MDISETMENU},
	{"WM_ENTERSIZEMOVE", WM_ENTERSIZEMOVE},
	{"WM_EXITSIZEMOVE", WM_EXITSIZEMOVE},
	{"WM_DROPFILES", WM_DROPFILES},
	{"WM_MDIREFRESHMENU", WM_MDIREFRESHMENU},
	{"WM_IME_SETCONTEXT", WM_IME_SETCONTEXT},
	{"WM_IME_NOTIFY", WM_IME_NOTIFY},
	{"WM_IME_CONTROL", WM_IME_CONTROL},
	{"WM_IME_COMPOSITIONFULL", WM_IME_COMPOSITIONFULL},
	{"WM_IME_SELECT", WM_IME_SELECT},
	{"WM_IME_CHAR", WM_IME_CHAR},
	{"WM_IME_REQUEST", WM_IME_REQUEST},
	{"WM_IME_KEYDOWN", WM_IME_KEYDOWN},
	{"WM_IME_KEYUP", WM_IME_KEYUP},
	{"WM_MOUSEHOVER", WM_MOUSEHOVER},
	{"WM_MOUSELEAVE", WM_MOUSELEAVE},
	{"WM_NCMOUSEHOVER", WM_NCMOUSEHOVER},
	{"WM_NCMOUSELEAVE", WM_NCMOUSELEAVE},
	{"WM_WTSSESSION_CHANGE", WM_WTSSESSION_CHANGE},
	{"WM_TABLET_FIRST", WM_TABLET_FIRST},
	{"WM_TABLET_LAST", WM_TABLET_LAST},
	{"WM_CUT", WM_CUT},
	{"WM_COPY", WM_COPY},
	{"WM_PASTE", WM_PASTE},
	{"WM_CLEAR", WM_CLEAR},
	{"WM_UNDO", WM_UNDO},
	{"WM_RENDERFORMAT", WM_RENDERFORMAT},
	{"WM_RENDERALLFORMATS", WM_RENDERALLFORMATS},
	{"WM_DESTROYCLIPBOARD", WM_DESTROYCLIPBOARD},
	{"WM_DRAWCLIPBOARD", WM_DRAWCLIPBOARD},
	{"WM_PAINTCLIPBOARD", WM_PAINTCLIPBOARD},
	{"WM_VSCROLLCLIPBOARD", WM_VSCROLLCLIPBOARD},
	{"WM_SIZECLIPBOARD", WM_SIZECLIPBOARD},
	{"WM_ASKCBFORMATNAME", WM_ASKCBFORMATNAME},
	{"WM_CHANGECBCHAIN", WM_CHANGECBCHAIN},
	{"WM_HSCROLLCLIPBOARD", WM_HSCROLLCLIPBOARD},
	{"WM_QUERYNEWPALETTE", WM_QUERYNEWPALETTE},
	{"WM_PALETTEISCHANGING", WM_PALETTEISCHANGING},
	{"WM_PALETTECHANGED", WM_PALETTECHANGED},
	{"WM_HOTKEY", WM_HOTKEY},
	{"WM_PRINT", WM_PRINT},
	{"WM_PRINTCLIENT", WM_PRINTCLIENT},
	{"WM_APPCOMMAND", WM_APPCOMMAND},
	{"WM_THEMECHANGED", WM_THEMECHANGED},
	{"WM_HANDHELDFIRST", WM_HANDHELDFIRST},
	{"WM_HANDHELDLAST", WM_HANDHELDLAST},
	{"WM_AFXFIRST", WM_AFXFIRST},
	{"WM_AFXLAST", WM_AFXLAST},
	{"WM_PENWINFIRST", WM_PENWINFIRST},
	{"WM_PENWINLAST", WM_PENWINLAST},
	{"WM_APP", WM_APP},
	{"WM_USER", WM_USER},
	{NULL, 0}
};

const char* msg_name(UINT msg)
{
	for (int i=0; msg_name_map[i].name; i++) {
		if (msg_name_map[i].msg == msg) {
			return msg_name_map[i].name;
		}
	}
	return "WM_UNKNOWN";
}

HWND hCandWnd, hCompWnd, hCMenuWnd, hStatusWnd;
