#include "stdafx.h"
#include "processdialog.h"
#include <process.h>

UINT_PTR hThread=NULL;
static HWND hDlg=NULL;
HINSTANCE hInst;

const DWORD TIMERID_ANIMATION=10001;
const DWORD TIMER_ANIMATION_TIMEOUT=100;

void DrawTransparentBitmapSpecial(RECT *rc, HDC hdc, HBITMAP hBitmap, int xSize, int ySize, COLORREF cTransparentColor)
{
	BITMAP     bm;
	COLORREF   cColor;
	HBITMAP    bmAndBack, bmAndObject, bmAndMem, bmSave;
	HBITMAP    bmBackOld, bmObjectOld, bmMemOld, bmSaveOld;
	HDC        hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave;
	POINT      ptSize;

	hdcTemp = CreateCompatibleDC(hdc);
	SelectObject(hdcTemp, hBitmap);   // Select the bitmap

	GetObject(hBitmap, sizeof(BITMAP), (LPSTR)&bm);
	ptSize.x = bm.bmWidth;            // Get width of bitmap
	ptSize.y = bm.bmHeight;           // Get height of bitmap
	DPtoLP(hdcTemp, &ptSize, 1);      // Convert from device
									  // to logical points

	int xStart=(rc->right-rc->left)/2-xSize/2+(xSize-ptSize.x), yStart=(rc->bottom-rc->top)/2-ySize/2+(ySize-ptSize.y);

	// Create some DCs to hold temporary data.
	hdcBack   = CreateCompatibleDC(hdc);
	hdcObject = CreateCompatibleDC(hdc);
	hdcMem    = CreateCompatibleDC(hdc);
	hdcSave   = CreateCompatibleDC(hdc);

	// Create a bitmap for each DC. DCs are required for a number of
	// GDI functions.

	// Monochrome DC
	bmAndBack   = CreateBitmap(ptSize.x, ptSize.y, 1, 1, NULL);

	// Monochrome DC
	bmAndObject = CreateBitmap(ptSize.x, ptSize.y, 1, 1, NULL);

	bmAndMem    = CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);
	bmSave      = CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);

	// Each DC must select a bitmap object to store pixel data.
	bmBackOld   = (HBITMAP)SelectObject(hdcBack, bmAndBack);
	bmObjectOld = (HBITMAP)SelectObject(hdcObject, bmAndObject);
	bmMemOld    = (HBITMAP)SelectObject(hdcMem, bmAndMem);
	bmSaveOld   = (HBITMAP)SelectObject(hdcSave, bmSave);

	// Set proper mapping mode.
	SetMapMode(hdcTemp, GetMapMode(hdc));

	// Save the bitmap sent here, because it will be overwritten.
	BitBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SRCCOPY);

	// Set the background color of the source DC to the color.
	// contained in the parts of the bitmap that should be transparent
	cColor = SetBkColor(hdcTemp, cTransparentColor);

	// Create the object mask for the bitmap by performing a BitBlt
	// from the source bitmap to a monochrome bitmap.
	BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0,
		SRCCOPY);

	// Set the background color of the source DC back to the original
	// color.
	SetBkColor(hdcTemp, cColor);

	// Create the inverse of the object mask.
	BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0,
		NOTSRCCOPY);

	// Copy the background of the main DC to the destination.
	BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdc, xStart, yStart,
		SRCCOPY);

	// Mask out the places where the bitmap will be placed.
	BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);

	// Mask out the transparent colored pixels on the bitmap.
	BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcBack, 0, 0, SRCAND);

	// XOR the bitmap with the background on the destination DC.
	BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SRCPAINT);

	// Copy the destination to the screen.
	BitBlt(hdc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0,
		SRCCOPY);

	// Place the original bitmap back into the bitmap sent here.
	BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcSave, 0, 0, SRCCOPY);

	// Delete the memory bitmaps.
	DeleteObject(SelectObject(hdcBack, bmBackOld));
	DeleteObject(SelectObject(hdcObject, bmObjectOld));
	DeleteObject(SelectObject(hdcMem, bmMemOld));
	DeleteObject(SelectObject(hdcSave, bmSaveOld));

	// Delete the memory DCs.
	DeleteDC(hdcMem);
	DeleteDC(hdcBack);
	DeleteDC(hdcObject);
	DeleteDC(hdcSave);
	DeleteDC(hdcTemp);
}

int DlgProc(HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam)
{
	static frame;

	switch(msg)
	{
		case WM_INITDIALOG:
			{
				::hDlg=hDlg;
				BOOST_TEST(SetTimer(hDlg, TIMERID_ANIMATION, TIMER_ANIMATION_TIMEOUT, NULL));
				frame=0;

				RECT rc;
				GetWindowRect(hDlg, &rc);
				int sx=rc.right-rc.left, sy=rc.bottom-rc.top;

				SetWindowPos(hDlg, HWND_TOPMOST, GetSystemMetrics(SM_CXFULLSCREEN)/2-sx/2, 
					GetSystemMetrics(SM_CYFULLSCREEN)/2-sy/2, sx, sy, 0);

				return TRUE;
			}
		case WM_TIMER:
			{
				BOOST_ASSERT(wParam==TIMERID_ANIMATION);

				HWND ac=GetDlgItem(hDlg, IDC_ANIMATION);

				HBITMAP hBm=LoadBitmap(hInst, MAKEINTRESOURCE((IDB_BITMAP1+frame)));
				HDC hDCD=GetDC(ac);

				RECT rc;
				GetWindowRect(ac, &rc);
				OffsetRect(&rc, -rc.left, -rc.top);

				HBRUSH hBrOld=(HBRUSH)SelectObject(hDCD, (HBRUSH)SendMessage(ac, WM_CTLCOLORSTATIC, 0, 0));
				HPEN hPenOld=(HPEN)SelectObject(hDCD, GetStockObject(NULL_PEN));
				Rectangle(hDCD, 0,0,rc.right,rc.bottom);
				SelectObject(hDCD, hPenOld);
				SelectObject(hDCD, hBrOld);

				DrawTransparentBitmapSpecial(&rc, hDCD, hBm, 30, 32, RGB(0,0,0));
				BOOST_TEST(ReleaseDC(ac, hDCD));
				BOOST_TEST(DeleteObject(hBm));

				if(++frame>17)
					frame=0;

                return TRUE;
			}
		case WM_CLOSE:
			{
				BOOST_TEST(KillTimer(hDlg, TIMERID_ANIMATION));
				BOOST_TEST(EndDialog(hDlg, 0));
				return TRUE;
			}
	}

	return 0;
}

void thproc(void *)
{
	DialogBox(hInst, MAKEINTRESOURCE(IDD_DLGPROGRESS), NULL, (DLGPROC)DlgProc);
}

void StartProcessDialog(HINSTANCE hInstance)
{
	if(hDlg||hThread)
		EndProcessDialog();

	hInst=hInstance;

	hThread=_beginthread(thproc, 0, NULL);
	while(!hDlg)
		Sleep(0);
}

void EndProcessDialog()
{
	SendMessage(hDlg, WM_CLOSE, 0, 0);
	WaitForSingleObject((HANDLE)hThread, INFINITE);
	hDlg=NULL;
	hThread=NULL;
}

void SetProcessDialogCurrentFile(const char *file)
{
	BOOST_TEST(hDlg);

	string msg="Currently processing: ";msg+=file;
	SetDlgItemText(hDlg, IDC_CURRENTFILE, msg.c_str());
}