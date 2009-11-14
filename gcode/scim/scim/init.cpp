#include <windows.h>
#include <winerror.h>
#include <memory.h>
#include <immdev.h>
#include <imedefs.h>
#include <regstr.h>

#include <map>
#include <string>
#include <vector>
using namespace std;

map<string, vector<string>> g_quail_rules;

static void init_quail_rules()
{
	const int max_line = 8192;
	char buff[max_line];

	FILE* fp = fopen("Q:\\.emacs_d\\lisp\\quail\\wubi86.el", "rb");
	if (!fp) {
		bhjerr("Error: can't open quail file");
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
}

void PASCAL InitImeGlobalData(HINSTANCE hInstance)
{
	init_quail_rules();

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
