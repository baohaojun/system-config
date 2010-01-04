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
#define STATE_WIDTH 32
#define STATE_HEIGHT 32

	ptPos.x = get_wa_rect().right - STATE_WIDTH;
	ptPos.y = get_wa_rect().bottom - STATE_HEIGHT;

	if (!get_status_wnd(hUIWnd)) {
		HWND stat = CreateWindowEx(WS_EX_TOPMOST|WS_EX_TRANSPARENT, get_status_class_name().c_str(), NULL, WS_POPUP | WS_DISABLED,
									  ptPos.x, ptPos.y, STATE_WIDTH, STATE_HEIGHT,
									  hUIWnd, (HMENU) NULL, g_hInst, NULL);

		//ModifyStyleEx
		SetWindowLong (stat, GWL_EXSTYLE , 
					   GetWindowLong (stat , GWL_EXSTYLE ) | WS_EX_LAYERED);

		SetLayeredWindowAttributes(stat,
								   RGB(255, 255, 255),
								   100,
								   LWA_COLORKEY|LWA_ALPHA);
		
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

#define HUNG_TIMEOUT 250
static void GetWindowIcons(HWND hwnd, HICON* phIcon, HICON* phIconSm) 
{

	_ASSERT(phIcon);

	BOOL fIsHungApp = FALSE;

	HICON hIcon = NULL;
	if (!SendMessageTimeout(hwnd, WM_GETICON, ICON_BIG, 0, 
							SMTO_ABORTIFHUNG, HUNG_TIMEOUT, (PDWORD_PTR)&hIcon)) {
		DWORD dwErr = GetLastError();
		if (dwErr == 0 || dwErr == 1460) {
			fIsHungApp = TRUE;
			goto _HUNG_ICON;
		}
	}
	if (!hIcon) 
		hIcon = (HICON)(UINT_PTR)GetClassLongPtr(hwnd, GCLP_HICON);

	if (!hIcon) {
	_HUNG_ICON:		
		hIcon = LoadIcon(NULL, IDI_APPLICATION);
	}
	*phIcon = hIcon;

	if (phIconSm) {
		if (fIsHungApp)
			goto _HUNG_ICONSM;
		hIcon = NULL;
		if (!SendMessageTimeout(hwnd, WM_GETICON, ICON_SMALL, 0, 
								SMTO_ABORTIFHUNG, HUNG_TIMEOUT, (PDWORD_PTR)&hIcon)) {
			DWORD dwErr = GetLastError();
			if (dwErr == 0 || dwErr == 1460)
				goto _HUNG_ICONSM;
		}
		if (!hIcon) {
			if (!SendMessageTimeout(hwnd, WM_GETICON, ICON_SMALL2, 0, 
									SMTO_ABORTIFHUNG, HUNG_TIMEOUT, (PDWORD_PTR)&hIcon)) {
				DWORD dwErr = GetLastError();
				if (dwErr == 0 || dwErr == 1460)
					goto _HUNG_ICONSM;
			}
		}
		if (!hIcon) {
			hIcon = (HICON)(UINT_PTR)GetClassLongPtr(hwnd, GCLP_HICONSM);
		}
		if (hIcon) {
			*phIconSm = hIcon;
		} else {
		_HUNG_ICONSM:
			*phIconSm = *phIcon;
		}
	}
}

static HICON GetWindowIcons(HWND hwnd)
{
	HICON icon, icon_small;
	GetWindowIcons(hwnd, &icon, &icon_small);
	return icon;
}

static void PaintStatusWindow(HWND hWnd, HDC hdc)
{
	
	CRect rect;
	GetClientRect(hWnd, &rect);

	//hdc_with_font dc_lucida(hdc, L"Lucida Console", 12);
	// hdc = GetDC(NULL);
	// CPoint pt = rect.TopLeft();
	// ClientToScreen(hWnd, &pt);
	// rect.OffsetRect(pt);

	wstring name = to_wstring(g_ime_name);
	HWND hUIWnd = GetWindow(hWnd, GW_OWNER);
	input_context ic(hUIWnd);

	if (!ic || !ic->hWnd) {
		DrawText(hdc, name.c_str(), name.size(), &rect, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
	} else {
		HWND hWnd = ic->hWnd;
		while (GetParent(hWnd) && GetParent(hWnd) != hWnd) {
			hWnd = GetParent(hWnd);
		}
		HICON icon = GetWindowIcons(hWnd);
		SetBkColor(hdc, RGB(254, 254, 255));
		SetTextColor(hdc, RGB(22, 1, 33));
		
		DrawIconEx (hdc, 0, 0, icon, rect.Width(), rect.Height(), 0, NULL, DI_NORMAL); 
	}

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
			if (g_ime_name == ime_off) {
				SetLayeredWindowAttributes(hWnd,
										   RGB_TRANS_KEY,
										   100,
										   LWA_COLORKEY|LWA_ALPHA);
			} else {
				SetLayeredWindowAttributes(hWnd, 
										   RGB_TRANS_KEY,
										   200,
										   LWA_COLORKEY|LWA_ALPHA);
			}
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
