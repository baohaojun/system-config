#include <windows.h>
#include <immdev.h>
#include <imedefs.h>
#include "imewnd.h"
#include <algorithm>
using std::reverse;

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

#define beep() do {								\
		if (getenv("SCIM_DEBUG")) {				\
			BHJDEBUG(" Beep");					\
		}										\
		beep();									\
	} while (0)

struct comp_want_key_t {
	modifier_t mod;
	u32 vk;
};

comp_want_key_t empty_want_key_map[];

BOOL WINAPI ImeProcessKey(HIMC /*hIMC*/,
						  u32 vk, LPARAM scan_code,
						  CONST LPBYTE kbd_state)
{
	// BHJDEBUG(" vk is %x, scan_code is, cnt:%d, sc:0x%02x, ext:%s, prev:%s", 
	// 		 vk, 
	// 		 (scan_code&0xffff),
	// 		 (scan_code & (0xff<<16)) >> 16,
	// 		 scan_code & (1 << 24) ? "yes" : "no", //extended, e.g., right ctrl/shift/menu
	// 		 scan_code & (1<<30) ? "up" : "down");
//#define bhjreturn(ret) BHJDEBUG(" return " #ret); return ret
#define bhjreturn(ret) return ret
	vk = LOWORD(vk);
	modifier_t mod = get_mod_state();

	if (mod == mod_ctrl && vk == VK_OEM_5) { //C-\, always want to be able to toggle
		bhjreturn (true);
	}
		
	if (g_comp_str.size()) {//started composing, want every key
		bhjreturn (true);
	}

	if (g_ime_name == ime_off) { //english mode, want nothing except the toggle key above.
		bhjreturn (false);
	}

	for (int i=0; empty_want_key_map[i].mod; i++) {
		if (empty_want_key_map[i].mod == mod &&
			empty_want_key_map[i].vk == vk) {
			return true;
		}
	}

	char szAscii[2] = "";

	ToAscii(vk, scan_code, kbd_state, (LPWORD) szAscii, 0);

	if (isgraph(szAscii[0])) { //not started composing, and a graph, we want it, 'cause the composing might start
		bhjreturn (true);
	}
	bhjreturn (false); //not started composing, composing can only start with a graph?
}

struct handler_map_t{
	const char *const comp_begin;
	int (*handler) (input_context& ic, modifier_t mod, u32 vk, char c);
	operator bool () {
		return comp_begin != NULL;
	}
};

handler_map_t handler_map[];


u32 WINAPI
ImeToAsciiEx(u32 vk,
			 u32 scan_code,
			 CONST LPBYTE kbd_state,
			 LPTRANSMSGLIST lpTransBuf, u32 /*fuState*/, HIMC hIMC)
{

#define return_ic_msgs(a) a; bhjreturn(ic.return_ime_msgs())

	//char kbd_char = (char) (HIWORD(vk));
	vk = LOWORD(vk);
	if (vk == VK_SHIFT || vk == VK_CONTROL || vk == VK_MENU) {
		return 0; //no beep
	}

	input_context ic(hIMC, lpTransBuf->TransMsg, lpTransBuf->uMsgCount);

	if (!ic) {
		return_ic_msgs(0);
	}

	const modifier_t mod = get_mod_state();
	if (mod == mod_ctrl && vk == VK_OEM_5) { //my toggle key, made the same as emacs
		if (g_ime_name == ime_off) {
			g_ime_name = ime_on;
			return_ic_msgs(ic.add_update_status_msg());
		} else {
			g_ime_name = ime_off;
			comp_remove_all();
			ic.add_update_status_msg();

			return_ic_msgs(ic.add_show_comp_msg());
		}
	}

	if (mod == mod_ctrl && vk == 'G') {
			comp_remove_all();
			ic.add_show_comp_msg();
			return_ic_msgs(1);
	}

	unsigned char ascii[2] = "";
	ToAscii(vk, scan_code, kbd_state, (LPWORD)ascii, 0);
	const char c = ascii[0];

	if (g_comp_str.empty()) { //special case
		return handler_map[0].handler(ic, mod, vk, c);
	}

	for (int i = 1/*special*/; handler_map[i]; i++) {
		const char*const comp = handler_map[i].comp_begin;
		if (strncmp(g_comp_str.c_str(), comp, strlen(comp)) == 0) {
			return handler_map[i].handler(ic, mod, vk, c);
		}
	}

	//g_comp_str not empty
	if (g_comp_str[0] == ';') {
		if (isprint(c)) {
			comp_append_1(c);
			return_ic_msgs(ic.add_show_comp_msg());
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
			return_ic_msgs(ic.send_text(key));
		case VK_BACK:
			comp_remove_1();
			return_ic_msgs(ic.add_show_comp_msg());
		case VK_SPACE:

			if (cands.size()) {
				ic.send_text(cands[g_active_cand]);
				promote_cand_for_key(g_active_cand, key);
				comp_remove_all(); //this will change g_active_cand, must after using it
				return_ic_msgs(1);
			} else {
				comp_append_1(' ');
				return_ic_msgs(ic.add_show_comp_msg());
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
					return_ic_msgs(0);
				}
				ic.send_text(cands[index]);
				promote_cand_for_key(index, key);
				comp_remove_all();
				return_ic_msgs(1);
			} else if (vk >= 'A' && vk <= 'Z') {
				string new_key = key;
				new_key.push_back(c);

				if (map_has_key(g_quail_rules, new_key)) {
					const vector<string>& new_cands = g_quail_rules[new_key];
					if (new_cands.size() == 1 && !map_has_key(g_trans_rule, new_key)) {
						ic.send_text(new_cands[0]);
						comp_remove_all();
						return_ic_msgs(ic);
					}
				} else if (cands.size() && !map_has_key(g_trans_rule, key)) {
					ic.send_text(cands[g_active_cand]);
					promote_cand_for_key(g_active_cand, key);
					comp_remove_all();
				}
				comp_append_1(c);
				return_ic_msgs(ic.add_show_comp_msg());					
			}

		}
	} else if (mod == mod_ctrl) {
	again:
		switch (vk) {
		case 'N':
			if (g_last_cand >= cands.size() - 1) {
				beep();
				return_ic_msgs(0);
			} else {
				g_first_cand = g_last_cand + 1;
				g_active_cand = g_first_cand;
				ic.add_show_comp_msg();
				return_ic_msgs(1);
			}
		case 'P':
			if (g_first_cand == 0) {
				beep();
				return_ic_msgs(0);
			} else {
				g_last_cand = g_first_cand - 1;
				g_active_cand = g_last_cand;
				ic.add_show_comp_msg();
				return_ic_msgs(1);
			}
		case 'F':
			if (g_active_cand == g_last_cand) {
				vk = 'N';
				goto again;
			}
			g_active_cand++;
			ic.add_show_comp_msg();
			return_ic_msgs(1);
		case 'B':
			if (g_active_cand == g_first_cand) {
				vk = 'P';
				goto again;
			}
			g_active_cand--;
			ic.add_show_comp_msg();
			return_ic_msgs(1);
		case VK_CONTROL:
			return_ic_msgs(0);
		default:
			beep();
			return_ic_msgs(0);
		}
	} 
	
	if (isprint(c) && cands.size()) {
		string send = cands[g_active_cand];
		send.push_back(c);
		ic.send_text(send);
		promote_cand_for_key(g_active_cand, key);
		comp_remove_all();
		return_ic_msgs(1);
	} 

	if (isprint(c)) {
		comp_append_1(c);
		return_ic_msgs(ic.add_show_comp_msg());
	}
	beep();
	return_ic_msgs(0);
}

u32 g_first_cand, g_last_cand, g_active_cand;
const char *const ime_off = "E";
const char *const ime_on = "C";

template<class Col> list<typename Col::value_type> bhj_unique(const Col& col)
{
	map<Col::value_type, int> u_map;
	list<Col::value_type> res;
	for (Col::const_iterator i = col.begin(); i != col.end(); i++) {
		if (u_map[*i]) {
			continue;
		} else {
			u_map[*i] = 1;
			res.push_back(*i);
		}
	}
	return res;
}

template<class Col> string join(const string& sep, const Col& col)
{
	string res;
	for (Col::const_iterator i = col.begin(); /*empty*/; /*empty*/) {
		res += *i;
		i++;
		if (i != col.end()) {
			res += sep;
		} else {
			break;
		}
	}
	return res;
}

static list<string> reverse_wubi_key(const wstring& ws)
{
	list<string> res_error;
	res_error.push_back("????");

	size_t n = ws.size();
	if (n < 2) {
		return res_error;
	}

	string C1 = to_string(ws.substr(0, 1));
	string C2 = to_string(ws.substr(1, 1));
	string Cn = to_string(ws.substr(n-1, 1));
	
	string res;
	if (!map_has_key(g_reverse_rules, C1) || !map_has_key(g_reverse_rules, C2) || !map_has_key(g_reverse_rules, Cn)) {
		beep();
		return res_error;
	}
	
	list<string> res_list;
	if (n == 2) {
		for (size_t i=0; i < g_reverse_rules[C1].size(); i++) {
			for (size_t j=0; j < g_reverse_rules[C2].size(); j++) {
				res_list.push_back(g_reverse_rules[C1][i] + g_reverse_rules[C2][j]);
			}
		}
	} else {
		for (size_t i=0; i < g_reverse_rules[C1].size(); i++) {
			for (size_t j=0; j < g_reverse_rules[C2].size(); j++) {
				for (size_t k=0; k < g_reverse_rules[Cn].size(); k++) {
					res_list.push_back(
						g_reverse_rules[C1][i].substr(0, 1) + 
						g_reverse_rules[C2][j].substr(0, 1) +
						g_reverse_rules[Cn][k].substr(0, 2));
				}
			}
		}
	}
	
	if (res_list.empty()) {
		return res_error;
	}
	return bhj_unique(res_list);
}

static wstring g_ws_self_help;
static void self_help_comp(u32 n)
{
	wstring &ws = g_ws_self_help;
	ws.clear();
	for (list<wchar_t>::reverse_iterator i=g_history_list.rbegin(); i != g_history_list.rend(); i++) {
		if (*i) {
			ws.push_back(*i);
			if (--n == 0) {
				break;
			}
		}
	}
	reverse(ws.begin(), ws.end());
	g_comp_str = "!add word: (";
	g_comp_str += join(" ", reverse_wubi_key(ws));
	g_comp_str += ") -> ";
	g_comp_str += to_string(ws);
}

static int empty_handler(input_context& ic, modifier_t mod, u32 vk, char c)
{
	if (!isgraph(c)) {
		if (mod == mod_ctrl && vk == VK_OEM_PLUS) {
			self_help_comp(2);
			return_ic_msgs(ic.add_show_comp_msg()); 
		}
		beep();
		return_ic_msgs(0); //Error, ImeProcessKey guarantee has been broken?
	}

	if (isalpha(c) || c == ';') { //lower case, a-z, up case, these all starts the composition.
		comp_append_1(c);

		//FIXME if only one cand with this key, pop it up
		ic.add_msg(WM_IME_STARTCOMPOSITION, 0, 0);
		return_ic_msgs(ic.add_show_comp_msg());
	} else {
		string s;
		s.push_back(c);
		return_ic_msgs(ic.send_text(s));
	}
}

static int self_help_handler(input_context& ic, modifier_t mod, u32 vk, char /*c*/)
{
	if (mod == mod_ctrl && vk == 'B') {
		self_help_comp(g_ws_self_help.size()+1);
	} else if (mod == mod_ctrl && vk == 'F' && 
			   g_ws_self_help.size() > 2) {
		self_help_comp(g_ws_self_help.size() - 1);
	} else if (mod == mod_none && vk == VK_RETURN) {
		const list<string>& revs = reverse_wubi_key(g_ws_self_help);
		for (list<string>::const_iterator i = revs.begin(); i != revs.end(); i++) {
			self_make_cand_for_key(to_string(g_ws_self_help), *i);
		}
		comp_remove_all();
	} else {
		beep();
	}
	return_ic_msgs(ic.add_show_comp_msg());
}

static int normal_handler(input_context& ic, modifier_t mod, u32 vk, char /*c*/)
{
	return_ic_msgs(0);
}

static handler_map_t handler_map[] = {
	{"", empty_handler},
	{"!add word", self_help_handler},
	{NULL, normal_handler}
};

static comp_want_key_t empty_want_key_map[] = {
	{mod_ctrl, 'G'},
	{mod_ctrl, VK_OEM_PLUS},
	{mod_ctrl, 'Q'},
	{mod_none, 0}
};
