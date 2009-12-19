#include <windows.h>
#include <winerror.h>
#include <memory.h>
#include <immdev.h>
#include <imedefs.h>
#include <regstr.h>
#include "imewnd.h"
#include <map>
#include <string>
#include <vector>

#include "ime-socket.h"


using namespace std;

rule_map_t g_quail_rules;
rule_map_t g_reverse_rules;
cand_hist_t g_cand_hist;
rule_trans_t g_trans_rule;

//FIXME what about the quail transit map?

typedef UINT64 u64;

static u64
GetSystemTimeAsUINT64()
{
    FILETIME filetime;
    GetSystemTimeAsFileTime( &filetime );

    ULARGE_INTEGER large;
    memcpy( &large, &filetime, sizeof(FILETIME) );

    return large.QuadPart;
}

list<wchar_t> g_history_list;




static bool InitImeGlobalData(HINSTANCE hInstance)
{
	// if (!init_wsock()) {
	// 	BHJDEBUG(" Error: init_wsock failed");
	// 	return false;
	// }
	
	init_ime_socket();

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

	return true;

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
	if (!GetClassInfoEx(hInstance, get_ui_class_name().c_str(), &wcWndCls)) {

		wcWndCls.style = CS_IME;

		wcWndCls.lpfnWndProc = UIWndProc;

		wstring ui_class = get_ui_class_name();
		wcWndCls.lpszClassName = ui_class.c_str();

		RegisterClassEx(&wcWndCls);
	}

	wcWndCls.style = CS_IME | CS_HREDRAW | CS_VREDRAW;

	wcWndCls.hbrBackground = GetStockObject(WHITE_BRUSH);

	if (!GetClassInfoEx(hInstance, get_comp_class_name().c_str(), &wcWndCls)) {
		wcWndCls.lpfnWndProc = CompWndProc;
		wstring comp = get_comp_class_name();
		wcWndCls.lpszClassName = comp.c_str();
		RegisterClassEx(&wcWndCls);
	}

	wcWndCls.hbrBackground = CreateSolidBrush(RGB_TRANS_KEY);
	if (!GetClassInfoEx(hInstance, get_status_class_name().c_str(), &wcWndCls)) {
		wcWndCls.lpfnWndProc = StatusWndProc;
		wstring stat = get_status_class_name();
		wcWndCls.lpszClassName = stat.c_str();
		RegisterClassEx(&wcWndCls);
	}
}


BOOL CALLBACK DllMain(HINSTANCE hInstance,
					  DWORD fdwReason,
					  LPVOID lpvReserve)
{
	wchar_t buf[1024] = L"";
	GetModuleFileName(NULL, buf, 1023);
	string exe_name = to_string(buf);
	int n = exe_name.find_last_of("/\\");
	if (n != exe_name.npos) {
		exe_name = exe_name.substr(n);
		exe_name[0] = '/';
	}

	const char* exclude_exes[] = {
		"/notepad.exe",
		NULL,
	};

	for (int i=0; exclude_exes[i]; i++) {
		if (_stricmp(exe_name.c_str(), exclude_exes[i])) {
			return false;
		}		
	} 

	switch (fdwReason) {

	case DLL_PROCESS_ATTACH:
		g_ime_name = ime_off;

		if (!g_hInst && ! InitImeGlobalData(hInstance)) {
			return false;
		}

		RegisterImeClass(hInstance, hInstance);

		break;

	case DLL_PROCESS_DETACH:
		{

			WNDCLASSEX wcWndCls;

			if (GetClassInfoEx(hInstance, get_status_class_name().c_str(), &wcWndCls)) {
				UnregisterClass(get_status_class_name().c_str(), hInstance);
			}

			if (GetClassInfoEx(hInstance, get_comp_class_name().c_str(), &wcWndCls)) {
				UnregisterClass(get_comp_class_name().c_str(), hInstance);
			}

			if (GetClassInfoEx(hInstance, get_ui_class_name().c_str(), &wcWndCls)) {
				UnregisterClass(get_ui_class_name().c_str(), hInstance);
			}
		}

		break;

	default:

		break;

	}

	return (TRUE);

}
