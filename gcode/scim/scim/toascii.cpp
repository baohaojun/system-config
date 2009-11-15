#include <windows.h>
#include <immdev.h>
#include <imedefs.h>
#include "imewnd.h"

typedef enum {
	mod_none = 0,
	mod_ctrl = 1,
	mod_shift = 2,
	mod_menu = 4,
	mod_ctrlshift = mod_ctrl | mod_shift,
	mod_ctrlmenu = mod_ctrl | mod_menu,
	mod_shiftmenu = mod_shift | mod_menu,
	mod_ctrlshiftmenu = mod_ctrl | mod_shift | mod_menu,
} modifier_t;


static modifier_t get_mod_state (const LPBYTE kbd_state)
{
	int mod = 0;
	if (GetKeyState(VK_CONTROL) & 0x8000) {
		mod |= mod_ctrl;
	}
	
	if (GetKeyState(VK_SHIFT) & 0x8000) {
		mod |= mod_shift;
	}

	if (GetKeyState(VK_MENU) & 0x8000) {
		mod |= mod_menu;
	}

	return (modifier_t)mod;
}

BOOL WINAPI ImeProcessKey(HIMC hIMC,
						  u32 vk, LPARAM scan_code,
						  CONST LPBYTE kbd_state)
{
	// BHJDEBUG(" vk is %x, scan_code is, cnt:%d, sc:0x%02x, ext:%s, prev:%s", 
	// 		 vk, 
	// 		 (scan_code&0xffff),
	// 		 (scan_code & (0xff<<16)) >> 16,
	// 		 scan_code & (1 << 24) ? "yes" : "no", //extended, e.g., right ctrl/shift/menu
	// 		 scan_code & (1<<30) ? "up" : "down");

	if (g_comp_str.size()) {
		return true;
	}

	BYTE szAscii[2];

	int nChars = ToAscii(vk, scan_code, kbd_state, (LPWORD) szAscii, 0);

	if (nChars != 1) {
		return false;
	}

	if (isgraph(szAscii[0])) {
		return true;
	}
	return false;
}


static u32 to_wm_char(
	u32 vk,
	u32 kbd_scan, 
	LPTRANSMSGLIST lpTransBuf, 
	PBYTE kbd_state
	)
{

	BYTE szAscii[4];

	int nChars = ToAscii(vk, kbd_scan, kbd_state,
					 (LPWORD) szAscii, 0);

	if (!nChars) {
		//BHJDEBUG(" ToAscii failed"); //but it's ok, we will return 0
	}

	LPTRANSMSG lpTransMsg = lpTransBuf->TransMsg;
	int i;
	for (i=0; i<nChars; i++) {

		lpTransMsg[i].message = WM_CHAR;
		lpTransMsg[i].wParam = szAscii[i];
		lpTransMsg[i].lParam = (kbd_scan << 16) | 1UL;
	}
	return i;
}

static void beep()
{
	MessageBeep(0xffffffffU);
}

static void reset_cand_index()
{
	g_first_cand = g_last_cand = g_active_cand = 0;
}

static void comp_append_1(char c)
{
	g_comp_str.push_back(c);
	reset_cand_index();
}

static void comp_remove_1()
{
	g_comp_str.resize(g_comp_str.size() - 1);
	reset_cand_index();
}

static void comp_remove_all()
{
	g_comp_str.erase();
	reset_cand_index();
}

#define beep() BHJDEBUG(" Beep"); beep()
u32 WINAPI
ImeToAsciiEx(u32 vk,
			 u32 scan_code,
			 CONST LPBYTE kbd_state,
			 LPTRANSMSGLIST lpTransBuf, u32 fuState, HIMC hIMC)
{
	char kbd_char = (char) (HIWORD(vk));
	vk = LOWORD(vk);
	input_context ic(hIMC, lpTransBuf->TransMsg, lpTransBuf->uMsgCount);

	if (!ic) {
		return to_wm_char(vk, scan_code, lpTransBuf, kbd_state);
	}

	if (!g_comp_str.size()) {
		unsigned char ascii[2];
		int n_ascii = ToAscii(vk, scan_code, kbd_state, (LPWORD)ascii, 0);
		if (n_ascii != 1) {
			beep();
			return 0; //error, 0 means no messages generated
		}

		char c = ascii[0];
		if (isalpha(c)) {
			comp_append_1(c);

			//FIXME if only one cand with this key, pop it up
			ic.add_msg(WM_IME_STARTCOMPOSITION, 0, 0);
			ic.add_show_comp_msg();
			return 2;
		} else {
			return to_wm_char(vk, scan_code, lpTransBuf, kbd_state);
		}
	}
	
	//g_comp_str not empty
	modifier_t mod = get_mod_state(kbd_state);
	string key = g_comp_str;

	//yeah, yeah, this is ugly, but we can't init a ref in the if,
	//because then we are out of the scope
	vector<string> empty_cands;
	vector<string> *pcands = &empty_cands; 
	if (g_quail_rules.find(g_comp_str) != g_quail_rules.end()) {
		pcands = &g_quail_rules[g_comp_str];
	}
	const vector<string>& cands = *pcands;
	//woo, end of ugly part

	if (mod == mod_none) {
		switch (vk) {
		case VK_RETURN:
			comp_remove_all();
			return ic.send_text(key);
		case VK_BACK:
			comp_remove_1();
			ic.add_show_comp_msg();
			return 1;
		case VK_SPACE:
			comp_remove_all();
			if (g_quail_rules.find(key) != g_quail_rules.end()) {
				return ic.send_text(g_quail_rules[key][0]);
			} else {
				beep();
				return 0;
			}
		default:
			if (vk >= 'A' && vk <= 'Z') {
				comp_append_1(vk-'A'+'a');
				ic.add_show_comp_msg();
				return 1;
			}
			if (vk >= '0' && vk <= '9') {
				u32 index = vk - '0';
				if (index == 0) {
					index += 10;
				}
				index -= 1;
				index += g_first_cand;

				if (index > g_last_cand || index > cands.size()) {
					beep();
					return 0;
				}
				comp_remove_all();
				return ic.send_text(g_quail_rules[key][index]);				
			}
		}
	} else if (mod == mod_ctrl) {
	again:
		switch (vk) {
		case 'G':
			comp_remove_all();
			ic.add_show_comp_msg();
			return 1;
		case 'N':
			if (g_last_cand >= cands.size() - 1) {
				beep();
				return 0;
			} else {
				g_first_cand = g_last_cand + 1;
				g_active_cand = g_first_cand;
				ic.add_show_comp_msg();
				return 1;
			}
		case 'P':
			if (g_first_cand == 0) {
				beep();
				return 0;
			} else {
				g_last_cand = g_first_cand - 1;
				g_active_cand = g_last_cand;
				ic.add_show_comp_msg();
				return 1;
			}
		case 'F':
			if (g_active_cand == g_last_cand) {
				vk = 'N';
				goto again;
			}
			g_active_cand++;
			ic.add_show_comp_msg();
			return 1;
		case 'B':
			if (g_active_cand == g_first_cand) {
				vk = 'P';
				goto again;
			}
			g_active_cand--;
			ic.add_show_comp_msg();
			return 1;
		case VK_CONTROL:
			return 0;
		default:
			beep();
			return 0;
		}
	}
			
	beep();
	return 0;
}

u32 g_first_cand, g_last_cand, g_active_cand;
