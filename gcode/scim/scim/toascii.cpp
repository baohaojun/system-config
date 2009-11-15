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


static modifier_t get_mod_state ()
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
#define bhjreturn(ret) BHJDEBUG(" return " #ret); return ret

	vk = LOWORD(vk);

	if (get_mod_state() == mod_ctrl && vk == VK_OEM_5) { //C-\, always want to be able to toggle
		bhjreturn (true);
	}
		
	if (g_comp_str.size()) {//started composing, want every key
		bhjreturn (true);
	}

	if (g_ime_name == ime_off) { //english mode, want nothing except the toggle key above.
		bhjreturn (false);
	}

	char szAscii[2] = "";

	int nChars = ToAscii(vk, scan_code, kbd_state, (LPWORD) szAscii, 0);

	if (isgraph(szAscii[0])) { //not started composing, and a graph, we want it, 'cause the composing might start
		bhjreturn (true);
	}
	bhjreturn (false); //not started composing, composing can only start with a graph?
}

u32 WINAPI
ImeToAsciiEx(u32 vk,
			 u32 scan_code,
			 CONST LPBYTE kbd_state,
			 LPTRANSMSGLIST lpTransBuf, u32 fuState, HIMC hIMC)
{
	char kbd_char = (char) (HIWORD(vk));
	vk = LOWORD(vk);
	if (vk == VK_SHIFT || vk == VK_CONTROL || vk == VK_MENU) {
		return 0; //no beep
	}

	input_context ic(hIMC, lpTransBuf->TransMsg, lpTransBuf->uMsgCount);

	if (!ic) {
		bhjreturn (0);
	}

	const modifier_t mod = get_mod_state();
	if (mod == mod_ctrl && vk == VK_OEM_5) { //my toggle key, made the same as emacs
		if (g_ime_name == ime_off) {
			g_ime_name = ime_on;
			ic.add_update_status_msg();
			bhjreturn (1);
		} else {
			g_ime_name = ime_off;
			comp_remove_all();
			ic.add_update_status_msg();
			ic.add_show_comp_msg();
			bhjreturn (2);
		}
	}

	unsigned char ascii[2] = "";
	int n_ascii = ToAscii(vk, scan_code, kbd_state, (LPWORD)ascii, 0);
	const char c = ascii[0];

	if (!g_comp_str.size()) { //not composing, ImeProcessKey has guarantee we only receive isgraph
		if (!isgraph(c)) {
			beep();
			bhjreturn (0); //Error, ImeProcessKey guarantee has been broken?
		}

		if (isalpha(c) || c == ';') {
			comp_append_1(c);

			//FIXME if only one cand with this key, pop it up
			ic.add_msg(WM_IME_STARTCOMPOSITION, 0, 0);
			ic.add_show_comp_msg();
			bhjreturn (2);
		} else {
			string s;
			s.push_back(c);
			bhjreturn (ic.send_text(s));
		}
	}
	
	//g_comp_str not empty
	if (g_comp_str[0] == ';') {
		if (isprint(c)) {
			comp_append_1(c);
			ic.add_show_comp_msg();
			return 1;
		}
	}
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
			if (key[0] == ';' && key.size() > 1) {
				key.erase(0, 1);
			}
			bhjreturn (ic.send_text(key));
		case VK_BACK:
			comp_remove_1();
			ic.add_show_comp_msg();
			bhjreturn (1);
		case VK_SPACE:
			comp_remove_all();
			if (g_quail_rules.find(key) != g_quail_rules.end()) {
				bhjreturn (ic.send_text(g_quail_rules[key][0]));
			} else {
				beep();
				bhjreturn (0);
			}
		default:
			if (vk >= '0' && vk <= '9') {
				u32 index = vk - '0';
				if (index == 0) {
					index += 10;
				}
				index -= 1;
				index += g_first_cand;

				if (index > g_last_cand || index > cands.size()) {
					beep();
					bhjreturn (0);
				}
				comp_remove_all();
				bhjreturn (ic.send_text(g_quail_rules[key][index]));				
			} else if (vk >= 'A' && vk <= 'Z') {
				int i = 0;
				if (key[0] >= 'a' && key[0] < 'z' && key.size() == 4 && cands.size()) { // 'z' is for pinyin
					i += ic.send_text(cands[0]);
					comp_remove_all();
				}
				comp_append_1(vk-'A'+'a');
				i += ic.add_show_comp_msg();
				bhjreturn (i);
			}

		}
	} else if (mod == mod_ctrl) {
	again:
		switch (vk) {
		case 'G':
			comp_remove_all();
			ic.add_show_comp_msg();
			bhjreturn (1);
		case 'N':
			if (g_last_cand >= cands.size() - 1) {
				beep();
				bhjreturn (0);
			} else {
				g_first_cand = g_last_cand + 1;
				g_active_cand = g_first_cand;
				ic.add_show_comp_msg();
				bhjreturn (1);
			}
		case 'P':
			if (g_first_cand == 0) {
				beep();
				bhjreturn (0);
			} else {
				g_last_cand = g_first_cand - 1;
				g_active_cand = g_last_cand;
				ic.add_show_comp_msg();
				bhjreturn (1);
			}
		case 'F':
			if (g_active_cand == g_last_cand) {
				vk = 'N';
				goto again;
			}
			g_active_cand++;
			ic.add_show_comp_msg();
			bhjreturn (1);
		case 'B':
			if (g_active_cand == g_first_cand) {
				vk = 'P';
				goto again;
			}
			g_active_cand--;
			ic.add_show_comp_msg();
			bhjreturn (1);
		case VK_CONTROL:
			bhjreturn (0);
		default:
			beep();
			bhjreturn (0);
		}
	}
	if (isprint(c) && cands.size()) {
		comp_remove_all();
		string text = cands[0];
		text.push_back(c);
		bhjreturn (ic.send_text(cands[0]+c));
	}		
	beep();
	bhjreturn (0);
}

u32 g_first_cand, g_last_cand, g_active_cand;
const char *const ime_off = "包包英文";
const char *const ime_on = "包包五笔";
