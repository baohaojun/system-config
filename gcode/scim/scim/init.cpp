
/*++

Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

Module Name:

    init.c

++*/

#include <windows.h>
#include <winerror.h>
#include <memory.h>
#include <immdev.h>
#include <imedefs.h>
#include <regstr.h>

int strbytelen(LPTSTR);

void PASCAL InitStatusUIData(int cxBorder, int cyBorder)
{

	int iContentHi;

	// iContentHi is to get the maximum value of predefined STATUS_DIM_Y and
	// a real Chinese character's height in the current HDC.

	iContentHi = STATUS_DIM_Y;

	if (iContentHi < sImeG.yChiCharHi)

		iContentHi = sImeG.yChiCharHi;

	// right bottom of status
	sImeG.rcStatusText.left = 0;

	sImeG.rcStatusText.top = 0;

	sImeG.rcStatusText.right =
		sImeG.rcStatusText.left +
		strbytelen(szImeName) * sImeG.xChiCharWi / 2 +
		STATUS_NAME_MARGIN + STATUS_DIM_X * 4;

	sImeG.rcStatusText.bottom = sImeG.rcStatusText.top + iContentHi;

	sImeG.xStatusWi =
		STATUS_DIM_X * 2 + STATUS_NAME_MARGIN +
		strbytelen(szImeName) * sImeG.xChiCharWi / 2 + 6 * cxBorder;

	sImeG.yStatusHi = iContentHi + 6 * cxBorder;

	// left bottom of imeicon bar
	sImeG.rcImeIcon.left = sImeG.rcStatusText.left;

	sImeG.rcImeIcon.top = sImeG.rcStatusText.top;

	sImeG.rcImeIcon.right = sImeG.rcImeIcon.left + STATUS_DIM_X;

	sImeG.rcImeIcon.bottom = sImeG.rcImeIcon.top + iContentHi;

	// left bottom of imename bar
	sImeG.rcImeName.left = sImeG.rcImeIcon.right;

	sImeG.rcImeName.top = sImeG.rcStatusText.top;

	sImeG.rcImeName.right =
		sImeG.rcImeName.left +
		strbytelen(szImeName) * sImeG.xChiCharWi / 2 + STATUS_NAME_MARGIN;

	sImeG.rcImeName.bottom = sImeG.rcImeName.top + iContentHi;

	sImeG.rcSymbol.left = sImeG.rcImeName.right;

	sImeG.rcSymbol.top = sImeG.rcStatusText.top;

	sImeG.rcSymbol.right = sImeG.rcSymbol.left + STATUS_DIM_X;

	sImeG.rcSymbol.bottom = sImeG.rcSymbol.top + iContentHi;

	return;

}

/**********************************************************************/
/* InitImeGlobalData()                                                */
/**********************************************************************/
void PASCAL InitImeGlobalData(HINSTANCE hInstance)
{

	int cxBorder, cyBorder;

	int UI_MODE;

	HDC hDC;

	TCHAR szChiChar[4];

	SIZE lTextSize;

	DWORD dwSize;

	HKEY hKeyIMESetting;

	LONG lRet;

	hInst = hInstance;

	LoadString(hInst, IDS_IMEREGNAME, szImeRegName, MAX_PATH);

	LoadString(hInst, IDS_IMENAME_UNI, pszImeName[0], MAX_PATH);

	// get the UI class name
	LoadString(hInst, IDS_IMEUICLASS, szUIClassName, CLASS_LEN);

	// get the composition class name
	LoadString(hInst, IDS_IMECOMPCLASS, szCompClassName, CLASS_LEN);

	// get the candidate class name

	// get the status class name
	LoadString(hInst, IDS_IMESTATUSCLASS, szStatusClassName, CLASS_LEN);

	//get the ContextMenu class name
	LoadString(hInst, IDS_IMECMENUCLASS, szCMenuClassName, CLASS_LEN);

	// work area
	SystemParametersInfo(SPI_GETWORKAREA, 0, &sImeG.rcWorkArea, 0);

	// border
	cxBorder = GetSystemMetrics(SM_CXBORDER);

	cyBorder = GetSystemMetrics(SM_CYBORDER);

	// get the Chinese char
	LoadString(hInst, IDS_CHICHAR, szChiChar,
			   sizeof(szChiChar) / sizeof(TCHAR));

	// get size of Chinese char
	hDC = GetDC(NULL);

	if (!GetTextExtentPoint
		(hDC, (LPTSTR) szChiChar, lstrlen(szChiChar), &lTextSize))

		memset(&lTextSize, 0, sizeof(SIZE));

	if (sImeG.rcWorkArea.right < 2 * UI_MARGIN) {

		sImeG.rcWorkArea.left = 0;

		sImeG.rcWorkArea.right = GetDeviceCaps(hDC, HORZRES);

	}

	if (sImeG.rcWorkArea.bottom < 2 * UI_MARGIN) {

		sImeG.rcWorkArea.top = 0;

		sImeG.rcWorkArea.bottom = GetDeviceCaps(hDC, VERTRES);

	}

	ReleaseDC(NULL, hDC);

	// get text metrics to decide the width & height of composition window
	// these IMEs always use system font to show
	sImeG.xChiCharWi = lTextSize.cx;

	sImeG.yChiCharHi = lTextSize.cy;

	UI_MODE = BOX_UI;

	InitStatusUIData(cxBorder, cyBorder);

	LoadString(hInst, IDS_STATUSERR, sImeG.szStatusErr,
			   sizeof(sImeG.szStatusErr) / sizeof(TCHAR));

	sImeG.cbStatusErr = lstrlen(sImeG.szStatusErr);

	sImeG.iCandStart = CAND_START;

	// get the UI offset for near caret operation
	RegCreateKey(HKEY_CURRENT_USER, szRegIMESetting, &hKeyIMESetting);

	dwSize = sizeof(DWORD);

	lRet = RegQueryValueEx(hKeyIMESetting,
						   szPara,
						   NULL, NULL, (LPBYTE) & sImeG.iPara, &dwSize);

	if (lRet != ERROR_SUCCESS) {

		sImeG.iPara = 0;

		RegSetValueEx(hKeyIMESetting,
					  szPara,
					  (DWORD) 0,
					  REG_BINARY, (LPBYTE) & sImeG.iPara, sizeof(int));

	}

	dwSize = sizeof(DWORD);

	lRet = RegQueryValueEx(hKeyIMESetting,
						   szPerp,
						   NULL, NULL, (LPBYTE) & sImeG.iPerp, &dwSize);

	if (lRet != ERROR_SUCCESS) {

		sImeG.iPerp = sImeG.yChiCharHi;

		RegSetValueEx(hKeyIMESetting,
					  szPerp,
					  (DWORD) 0,
					  REG_BINARY, (LPBYTE) & sImeG.iPerp, sizeof(int));

	}

	dwSize = sizeof(DWORD);

	lRet = RegQueryValueEx(hKeyIMESetting,
						   szParaTol,
						   NULL, NULL, (LPBYTE) & sImeG.iParaTol, &dwSize);

	if (lRet != ERROR_SUCCESS) {

		sImeG.iParaTol = sImeG.xChiCharWi * 4;

		RegSetValueEx(hKeyIMESetting,
					  szParaTol,
					  (DWORD) 0,
					  REG_BINARY, (LPBYTE) & sImeG.iParaTol, sizeof(int));

	}

	dwSize = sizeof(DWORD);

	lRet = RegQueryValueEx(hKeyIMESetting,
						   szPerpTol,
						   NULL, NULL, (LPBYTE) & sImeG.iPerpTol, &dwSize);

	if (lRet != ERROR_SUCCESS) {

		sImeG.iPerpTol = lTextSize.cy;

		RegSetValueEx(hKeyIMESetting,
					  szPerpTol,
					  (DWORD) 0,
					  REG_BINARY, (LPBYTE) & sImeG.iPerpTol, sizeof(int));

	}

	RegCloseKey(hKeyIMESetting);

	return;

}

BOOL PASCAL InitImeLocalData(HINSTANCE hInstL)
{

	int cxBorder, cyBorder;

	int iContentHi;

	// iContentHi is to get the maximum value of predefined COMP_TEXT_Y and
	// a real Chinese character's height in the current HDC.

	iContentHi = COMP_TEXT_Y;

	if (iContentHi < sImeG.yChiCharHi)

		iContentHi = sImeG.yChiCharHi;

	lpImeL->hInst = hInstL;

	lpImeL->nMaxKey = 4;

	// border + raising edge + sunken edge
	cxBorder = GetSystemMetrics(SM_CXBORDER);

	cyBorder = GetSystemMetrics(SM_CYBORDER);

	// text position relative to the composition window
	lpImeL->cxCompBorder = cxBorder * 2;

	lpImeL->cyCompBorder = cyBorder * 2;


	lpImeL->xCompWi = 400;


	lpImeL->yCompHi = 60;

	// default position of composition window
	lpImeL->ptDefComp.x =
		sImeG.rcWorkArea.right - lpImeL->xCompWi - cxBorder * 2;

	lpImeL->ptDefComp.y =
		sImeG.rcWorkArea.bottom - lpImeL->yCompHi - cyBorder * 2;

	lpImeL->fModeConfig = MODE_CONFIG_QUICK_KEY | MODE_CONFIG_PREDICT;

	return (TRUE);

}


/**********************************************************************/
/* RegisterIme()                                                      */
/**********************************************************************/
void PASCAL RegisterIme(HINSTANCE hInstance)
{

	lstrcpy(sImeG.UsedCodes, TEXT("0123456789abcdef"));

	sImeG.wNumCodes = (WORD) lstrlen(sImeG.UsedCodes);

	sImeG.IC_Enter = 0;
	return;

}

void PASCAL RegisterImeClass(HINSTANCE hInstance, HINSTANCE hInstL)
{

	WNDCLASSEX wcWndCls;

	// IME UI class
	// Register IME UI class
	wcWndCls.cbSize = sizeof(WNDCLASSEX);

	wcWndCls.cbClsExtra = 0;

	wcWndCls.cbWndExtra = sizeof(INT_PTR) * 2;

	wcWndCls.hIcon =
		LoadImage(hInstL, MAKEINTRESOURCE(IDI_IME),
				  IMAGE_ICON, 32, 32, LR_DEFAULTCOLOR);

	wcWndCls.hInstance = hInstance;

	wcWndCls.hCursor = LoadCursor(NULL, IDC_ARROW);

	wcWndCls.hbrBackground = GetStockObject(NULL_BRUSH);

	wcWndCls.lpszMenuName = (LPTSTR) NULL;

	wcWndCls.hIconSm =
		LoadImage(hInstL, MAKEINTRESOURCE(IDI_IME),
				  IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);

	// IME UI class
	if (!GetClassInfoEx(hInstance, szUIClassName, &wcWndCls)) {

		wcWndCls.style = CS_IME;

		wcWndCls.lpfnWndProc = UIWndProc;

		wcWndCls.lpszClassName = (LPTSTR) szUIClassName;

		RegisterClassEx(&wcWndCls);

	}

	wcWndCls.style = CS_IME | CS_HREDRAW | CS_VREDRAW;

	wcWndCls.hbrBackground = GetStockObject(LTGRAY_BRUSH);

	// IME composition class
	// register IME composition class
	if (!GetClassInfoEx(hInstance, szCompClassName, &wcWndCls)) {

		wcWndCls.lpfnWndProc = CompWndProc;

		wcWndCls.lpszClassName = (LPTSTR) szCompClassName;

		RegisterClassEx(&wcWndCls);

	}

	// IME status class
	// register IME status class
	if (!GetClassInfoEx(hInstance, szStatusClassName, &wcWndCls)) {

		wcWndCls.lpfnWndProc = StatusWndProc;

		wcWndCls.lpszClassName = (LPTSTR) szStatusClassName;

		RegisterClassEx(&wcWndCls);

	}

	return;

}


/**********************************************************************/
/* ImeDllInit()                                                       */
/* Return Value:                                                      */
/*      TRUE - successful                                             */
/*      FALSE - failure                                               */
/**********************************************************************/
BOOL CALLBACK DllMain(HINSTANCE hInstance,	// instance handle of this library
					  DWORD fdwReason,	// reason called
					  LPVOID lpvReserve)	// reserve pointer
{

	switch (fdwReason) {

	case DLL_PROCESS_ATTACH:

		RegisterIme(hInstance);

		// init globaldata & load globaldata from resource
		sImeL.dwRegImeIndex = 0;
		szImeName = pszImeName[sImeL.dwRegImeIndex];

		if (!hInst) {
			InitImeGlobalData(hInstance);
		}

		if (!lpImeL) {

			lpImeL = &sImeL;

			InitImeLocalData(hInstance);

		}

		RegisterImeClass(hInstance, hInstance);

		break;

	case DLL_PROCESS_DETACH:
		{

			WNDCLASSEX wcWndCls;

			if (GetClassInfoEx(hInstance, szCMenuClassName, &wcWndCls)) {

				UnregisterClass(szCMenuClassName, hInstance);

			}

			if (GetClassInfoEx(hInstance, szStatusClassName, &wcWndCls)) {

				UnregisterClass(szStatusClassName, hInstance);

			}

			if (GetClassInfoEx(hInstance, szCompClassName, &wcWndCls)) {

				UnregisterClass(szCompClassName, hInstance);

			}

			if (GetClassInfoEx(hInstance, szUIClassName, &wcWndCls) && UnregisterClass(szUIClassName, hInstance)) {

				DestroyIcon(wcWndCls.hIcon);
				DestroyIcon(wcWndCls.hIconSm);

			}

		}

		break;

	default:

		break;

	}

	return (TRUE);

}

int strbytelen(LPTSTR lpStr) //used for calculating text size
{

	int i, len, iRet;

	len = lstrlen(lpStr);

	for (i = 0, iRet = 0; i < len; i++, iRet++) {

		if (lpStr[i] > 0x100)

			iRet++;

	}

	return iRet;

}
