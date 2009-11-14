#include <windows.h>
#include <winerror.h>
#include <memory.h>
#include <immdev.h>
#include <imedefs.h>
#include <regstr.h>

void PASCAL InitImeGlobalData(HINSTANCE hInstance)
{
	TCHAR szChiChar[4] = {0x9999, 0};

	SIZE lTextSize;

	g_hInst = hInstance;

	HDC hDC = GetDC(NULL);

	if (!GetTextExtentPoint (hDC, (LPTSTR) szChiChar, 1, &lTextSize)) {
		memset(&lTextSize, 0, sizeof(SIZE));
	}
	ReleaseDC(NULL, hDC);

	sImeG.xChiCharWi = lTextSize.cx;
	sImeG.yChiCharHi = lTextSize.cy;

	sImeG.iPara = 0;
	sImeG.iPerp = sImeG.yChiCharHi;

	sImeG.iParaTol = sImeG.xChiCharWi * 4;
	sImeG.iPerpTol = lTextSize.cy;

	return;

}

void PASCAL RegisterImeClass(HINSTANCE hInstance, HINSTANCE hInstL)
{

	WNDCLASSEX wcWndCls;


	wcWndCls.cbSize = sizeof(WNDCLASSEX);

	wcWndCls.cbClsExtra = 0;

	wcWndCls.cbWndExtra = sizeof(INT_PTR) * 2;

	wcWndCls.hIcon = NULL;

	wcWndCls.hInstance = hInstance;

	wcWndCls.hCursor = NULL;

	wcWndCls.hbrBackground = GetStockObject(NULL_BRUSH);

	wcWndCls.lpszMenuName = (LPTSTR) NULL;

	wcWndCls.hIconSm = NULL;

	// IME UI class
	if (!GetClassInfoEx(hInstance, szUIClassName, &wcWndCls)) {

		wcWndCls.style = CS_IME;

		wcWndCls.lpfnWndProc = UIWndProc;

		wcWndCls.lpszClassName = (LPTSTR) szUIClassName;

		RegisterClassEx(&wcWndCls);

	}

	wcWndCls.style = CS_IME | CS_HREDRAW | CS_VREDRAW;

	wcWndCls.hbrBackground = GetStockObject(WHITE_BRUSH);

	// IME composition class
	// register IME composition class
	if (!GetClassInfoEx(hInstance, szCompClassName, &wcWndCls)) {

		wcWndCls.lpfnWndProc = CompWndProc;

		wcWndCls.lpszClassName = (LPTSTR) szCompClassName;

		RegisterClassEx(&wcWndCls);

	}

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
		szImeName = L"\x5305\x5305\x4e94\x7b14"; //包包五笔

		if (!g_hInst) {
			InitImeGlobalData(hInstance);
		}

		RegisterImeClass(hInstance, hInstance);

		break;

	case DLL_PROCESS_DETACH:
		{

			WNDCLASSEX wcWndCls;

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
