
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

void show_status_wnd()
{
	ShowWindow(g_hStatusWnd, SW_SHOWNOACTIVATE);
}

void hide_status_wnd()
{
	ShowWindow(g_hStatusWnd, SW_HIDE);
}

void PASCAL OpenStatus(HWND hUIWnd)
{
	POINT ptPos;
#define STATE_WIDTH 70
#define STATE_HEIGHT 25

	ptPos.x = get_wa_rect().right - STATE_WIDTH;
	ptPos.y = get_wa_rect().bottom - STATE_HEIGHT;

	if (!g_hStatusWnd) {
		g_hStatusWnd = CreateWindowEx(0, szStatusClassName, NULL, WS_POPUP | WS_DISABLED,
									  ptPos.x, ptPos.y, STATE_WIDTH, STATE_HEIGHT,
									  hUIWnd, (HMENU) NULL, hInst,
									  NULL);
	}

	input_context ic(hUIWnd);
	if (!ic) {
		hide_status_wnd();
	} else {
		show_status_wnd();
	}
	return;
}

static void DestroyStatusWindow()
{
	g_hStatusWnd = NULL;
	return;
}

static void PaintStatusWindow(HDC hDC)
{
	
	CRect rect;
	GetClientRect(g_hStatusWnd, &rect);

	SetTextColor(hDC, RGB(0x00, 0x00, 0x00));

	SetBkColor(hDC, RGB(0xff, 0xff, 0xff));

	DrawText(hDC, szImeName, lstrlen(szImeName),
			 &rect, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
}

LRESULT CALLBACK
StatusWndProc(HWND hWnd, u32 uMsg, WPARAM wParam, LPARAM lParam)
{
	//BHJDEBUG("received msg %s", msg_name(uMsg));
	if (!g_hStatusWnd) {
		g_hStatusWnd = hWnd;
	} else if (g_hStatusWnd != hWnd) {
		BHJDEBUG(" Error: hWnd %x not g_hStatusWnd %x", hWnd, g_hStatusWnd);
		exit(-1);
	}		
	
	switch (uMsg) {
	case WM_DESTROY:
		DestroyStatusWindow();
		break;
	case WM_IME_NOTIFY:
		break;
	case WM_PAINT:
		{
			HDC hDC;
			PAINTSTRUCT ps;

			hDC = BeginPaint(g_hStatusWnd, &ps);
			PaintStatusWindow(hDC);
			EndPaint(g_hStatusWnd, &ps);
		}
		break;
	case WM_MOUSEACTIVATE:
		return (MA_NOACTIVATE);
	default:
		//BHJDEBUG(" msg %s not handled", msg_name(uMsg));
		return DefWindowProc(g_hStatusWnd, uMsg, wParam, lParam);
	}

	return (0L);
}

