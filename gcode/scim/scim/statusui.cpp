#include <windows.h>
#include <immdev.h>
#include <htmlhelp.h>
#include <string.h>
#include <regstr.h>
#include <imedefs.h>
#include <resource.h>
#include "imewnd.h"
extern HWND hCrtDlg;

void show_status_wnd(HWND hUIWnd)
{
	ShowWindow(get_status_wnd(hUIWnd), SW_SHOWNOACTIVATE);
	RedrawWindow(get_status_wnd(hUIWnd), NULL, NULL, RDW_FRAME | RDW_INVALIDATE | RDW_ERASE);
}

void hide_status_wnd(HWND hUIWnd)
{
	ShowWindow(get_status_wnd(hUIWnd), SW_HIDE);
}

void PASCAL OpenStatus(HWND hUIWnd)
{
	POINT ptPos;
#define STATE_WIDTH 20
#define STATE_HEIGHT 15

	ptPos.x = get_wa_rect().right - STATE_WIDTH;
	ptPos.y = get_wa_rect().bottom - STATE_HEIGHT;

	if (!get_status_wnd(hUIWnd)) {
		HWND stat = CreateWindowEx(WS_EX_TOPMOST, szStatusClassName, NULL, WS_POPUP | WS_DISABLED,
									  ptPos.x, ptPos.y, STATE_WIDTH, STATE_HEIGHT,
									  hUIWnd, (HMENU) NULL, g_hInst, NULL);
		set_status_wnd(hUIWnd, stat);
	}

	input_context ic(hUIWnd);
	if (!ic) {
		hide_status_wnd(hUIWnd);
	} else {
		show_status_wnd(hUIWnd);
	}
	return;
}

static void PaintStatusWindow(HWND hWnd, HDC hDC)
{
	
	CRect rect;
	GetClientRect(hWnd, &rect);
	wstring name = to_wstring(g_ime_name);
	DrawText(hDC, name.c_str(), name.size(), &rect, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
}

LRESULT CALLBACK
StatusWndProc(HWND hWnd, u32 uMsg, WPARAM wParam, LPARAM lParam)
{
	//BHJDEBUG("received msg %s", msg_name(uMsg));
	
	switch (uMsg) {
	case WM_CREATE:
		break;
	case WM_DESTROY:
		break;
	case WM_IME_NOTIFY:
		break;
	case WM_PAINT:
		{
			HDC hDC;
			PAINTSTRUCT ps;

			hDC = BeginPaint(hWnd, &ps);
			PaintStatusWindow(hWnd, hDC);
			EndPaint(hWnd, &ps);
		}
		break;
	case WM_MOUSEACTIVATE:
		return (MA_NOACTIVATE);
	default:
		//BHJDEBUG(" msg %s not handled", msg_name(uMsg));
		return DefWindowProc(hWnd, uMsg, wParam, lParam);
	}

	return (0L);
}

string g_ime_name;
