#include <windows.h>
#include <winerror.h>
#include <memory.h>
#include <immdev.h>
#include <imedefs.h>

#include "imewnd.h"
#include <map>
#include <string>
#include <vector>

#include "ime-socket.h"

using namespace std;

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

static bool InitImeGlobalData(HINSTANCE hInstance)
{
	g_ui_private = new ui_private_t; //this is a hack, if g_ui_private is not dynamic, it seems its dtor will be called too ealy. I don't know why.
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


static vector<string> get_enable_only_list()
{
	vector<string> res;
	FILE *fp = fopen("c:/etc/ywb/enable_only.rc", "rb");
	if (!fp) {
		return res; //res is empty
	}

	char buff[1024];
	while(fgets(buff, 1024, fp)) {
		buff[strlen(buff) - 1] = 0;
		res.push_back(buff);
	}
	fclose(fp);
	if (!res.size()) {//make sure res is not empty, basically this
					  //disable all process from using our IME,
					  //because the only process enabled is "no such
					  //program.exe".
		res.push_back("no such program.exe"); 
	}
	return res;
}

static vector<string> get_disable_list()
{
	vector<string> res;
	res.push_back("xwin.exe");
	res.push_back("logonui.exe");
	res.push_back("conime.exe");

	FILE *fp = fopen("c:/etc/ywb/disable.rc", "rb");
	if (!fp) {
		return res;
	}

	char buff[1024];
	while (fgets(buff, 1024, fp)) {
		buff[strlen(buff) - 1] = 0;
		res.push_back(buff);
	}
	fclose(fp);
	return res;
}

static bool calling_process_ok()
{
	wchar_t buf[1024] = L"";
	GetModuleFileName(NULL, buf, 1023);
	string exe_name = to_string(buf);
	int n = exe_name.find_last_of("/\\");
	if (n != exe_name.npos) {
		exe_name = exe_name.substr(n+1);
	}

	vector<string> enable_list = get_enable_only_list();
	if (enable_list.size()) {
		for (vector<string>::iterator i = enable_list.begin(); i != enable_list.end(); i++) {
			if (!_stricmp(exe_name.c_str(), i->c_str())) {
				return true;
			}
		}
		return false; //if there is a enable list, and the calling process is not in it, that means it's excluded.
	}

	vector<string> disable_list = get_disable_list();
	if (disable_list.size()) {
		for (vector<string>::iterator i = disable_list.begin(); i != disable_list.end(); i++) {
			if (!_stricmp(exe_name.c_str(), i->c_str())) {
				return false;
			}
		}
	}
	return true;
}

BOOL CALLBACK DllMain(HINSTANCE hInstance,
					  DWORD fdwReason,
					  LPVOID lpvReserve)
{

	if (fdwReason == DLL_PROCESS_ATTACH) {
		if (! calling_process_ok()) {
			BHJDEBUG("not ok.");
			if (!g_hInst) {
				g_hInst = hInstance; //hack, so that the class names are unique.
			}
			return false;
		} else {
			BHJDEBUG("ok");
		}
	}

	switch (fdwReason) {

	case DLL_PROCESS_ATTACH:

		if (!g_hInst && ! InitImeGlobalData(hInstance)) {
			return false;
		}

		RegisterImeClass(hInstance, hInstance);

		break;

	case DLL_PROCESS_DETACH:

		break;

	default:

		break;

	}

	return (TRUE);

}
