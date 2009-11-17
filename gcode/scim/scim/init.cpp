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
using namespace std;

rule_map_t g_quail_rules;
rule_map_t g_reverse_rules;

list<wchar_t> g_history_list;
static bool init_quail_rules()
{
	const int max_line = 8192;
	char buff[max_line];
	for (int i=0; i<100; i++) {
		g_history_list.push_back(0);
	}

	FILE* fp = fopen("Q:\\.emacs_d\\lisp\\quail\\wubi86.el", "rb");
	if (!fp) {
		BHJDEBUG(" Error: can't open quail file");
		return false;
	}

	enum {
		rule_begin,
		rule_defining,
		rule_end,
	} state = rule_begin;

	while (fgets(buff, max_line, fp)) {
		if (state == rule_begin && strstr(buff, "quail-define-rules")) {
			state = rule_defining;
			continue;
		}

		if (state == rule_defining) {
			int quote = 0;
			string key_rule;
			string key;
			for (int i=0; buff[i]; i++) {
				if (buff[i] == '"') {
					quote++;
					if (quote % 2) {
						key_rule = "";
					} else if (quote == 2) {
						key = key_rule;
					} else {
						g_quail_rules[key].push_back(key_rule);
					}
				} else {
					if (quote % 2) {
						key_rule.push_back(buff[i]);
					}
				}
			}

			if (quote == 0) { //we hit a line there is no quote, must been stopped
				break;
			}
		}
	}
	fclose(fp);
	fp = NULL;
	fp = fopen("Q:\\.emacs_d\\lisp\\quail\\reverse.txt", "rb");
	if (!fp) {
		BHJDEBUG(" Error: can't open reverse.txt");
		return true; //if we get here, at least we can input.
	}
	
	while (fgets(buff, max_line, fp)) {

		int quote = 0;
		string key_rule;
		string key;
		for (int i=0; buff[i]; i++) {
			if (buff[i] == '"') {
				quote++;
				if (quote % 2) {
					key_rule = "";
				} else if (quote == 2) {
					key = key_rule;
				} else {
					g_reverse_rules[key].push_back(key_rule);
				}
			} else {
				if (quote % 2) {
					key_rule.push_back(buff[i]);
				}
			}
		}
	}
	fclose(fp);
	return true;
}

static bool InitImeGlobalData(HINSTANCE hInstance)
{
	if (!init_quail_rules()) {
		return false;
	}

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
	if (!GetClassInfoEx(hInstance, szUIClassName, &wcWndCls)) {

		wcWndCls.style = CS_IME;

		wcWndCls.lpfnWndProc = UIWndProc;

		wcWndCls.lpszClassName = (LPTSTR) szUIClassName;

		RegisterClassEx(&wcWndCls);

	}

	wcWndCls.style = CS_IME | CS_HREDRAW | CS_VREDRAW;

	wcWndCls.hbrBackground = GetStockObject(WHITE_BRUSH);

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
}


BOOL CALLBACK DllMain(HINSTANCE hInstance,
					  DWORD fdwReason,
					  LPVOID lpvReserve)
{

	wchar_t buf[1024] = L"";
	GetModuleFileName(NULL, buf, 1023);
	string exe_name = to_string(buf);
	if (strcasestr(exe_name.c_str(), "xwin.exe")) {
		BHJDEBUG("Error: xwin.exe is calling, they can't handle IME!");
		return false;
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

			if (GetClassInfoEx(hInstance, szStatusClassName, &wcWndCls)) {
				UnregisterClass(szStatusClassName, hInstance);
			}

			if (GetClassInfoEx(hInstance, szCompClassName, &wcWndCls)) {
				UnregisterClass(szCompClassName, hInstance);
			}

			if (GetClassInfoEx(hInstance, szUIClassName, &wcWndCls)) {
				UnregisterClass(szUIClassName, hInstance);
			}
		}

		break;

	default:

		break;

	}

	return (TRUE);

}
