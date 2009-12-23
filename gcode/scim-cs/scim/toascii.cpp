#include <windows.h>
#include <immdev.h>
#include <imedefs.h>
#include "imewnd.h"
#include <algorithm>
#include "ime-socket.h"
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

	//u64 ts = GetSystemTimeAsUINT64();
	while (fgets(buff, max_line, fp)) {
		if (state == rule_begin && strstr(buff, "quail-define-rules")) {
			state = rule_defining;
			continue;
		}

		if (state == rule_defining) {
			int quote = 0;
			char * key;
			char * data;

			for (int i=0; buff[i]; i++) {
				if (buff[i] == '"') {
					buff[i] = 0;
					quote++;
					if (quote % 2) {
						data = buff + i + 1;
					} else if (quote == 2) {
						key = data;
						string trans = key;
						for (size_t i=1; i < trans.size(); i++) {
							g_trans_rule[trans.substr(0, i)][trans.substr(i, 1)] = 1;
						}
					} else {
						g_quail_rules[key].push_back(data);
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
		char * key;
		char * data;

		for (int i=0; buff[i]; i++) {
			if (buff[i] == '"') {
				buff[i] = 0;
				quote++;
				if (quote % 2) {
					data = buff + i + 1;
				} else if (quote == 2) {
					key = data;
				} else {
					g_reverse_rules[key].push_back(data);
				}
			}
		}
	}
	fclose(fp);
	return true;
}

static const char *special_keys[256] = {
    NULL,                // 0                      

    NULL,                // VK_LBUTTON        0x01 
    NULL,                // VK_RBUTTON        0x02 
    "cancel",         // VK_CANCEL         0x03 
    NULL,                // VK_MBUTTON        0x04 

    NULL, NULL, NULL,          //    0x05 .. 0x07        

    "backspace",      // VK_BACK           0x08 
    "tab",            // VK_TAB            0x09 

    NULL, NULL,             //    0x0A .. 0x0B        

    "clear",          // VK_CLEAR          0x0C 
    "return",         // VK_RETURN         0x0D 

    NULL, NULL,             //    0x0E .. 0x0F        

    NULL,                // VK_SHIFT          0x10 
    NULL,                // VK_CONTROL        0x11 
    NULL,                // VK_MENU           0x12 
    "pause",          // VK_PAUSE          0x13 
    "capslock",       // VK_CAPITAL        0x14 
    "kana",           // VK_KANA/VK_HANGUL 0x15 
    NULL,                //    0x16                
    "junja",          // VK_JUNJA          0x17 
    "final",          // VK_FINAL          0x18 
    "kanji",          // VK_KANJI/VK_HANJA 0x19 
    NULL,                //    0x1A                
    "escape",         // VK_ESCAPE         0x1B 
    "convert",        // VK_CONVERT        0x1C 
    "non-convert",    // VK_NONCONVERT     0x1D 
    "accept",         // VK_ACCEPT         0x1E 
    "mode-change",    // VK_MODECHANGE     0x1F 
    'space',                // VK_SPACE          0x20 
    "prior",          // VK_PRIOR          0x21 
    "next",           // VK_NEXT           0x22 
    "end",            // VK_END            0x23 
    "home",           // VK_HOME           0x24 
    "left",           // VK_LEFT           0x25 
    "up",             // VK_UP             0x26 
    "right",          // VK_RIGHT          0x27 
    "down",           // VK_DOWN           0x28 
    "select",         // VK_SELECT         0x29 
    "print",          // VK_PRINT          0x2A 
    "execute",        // VK_EXECUTE        0x2B 
    "snapshot",       // VK_SNAPSHOT       0x2C 
    "insert",         // VK_INSERT         0x2D 
    "delete",         // VK_DELETE         0x2E 
    "help",           // VK_HELP           0x2F 

    // VK_0 thru VK_9 are the same as ASCII '0' thru '9' (0x30 - 0x39) 

    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,

    NULL, NULL, NULL, NULL, NULL, NULL, NULL, // 0x3A .. 0x40       

    // VK_A thru VK_Z are the same as ASCII 'A' thru 'Z' (0x41 - 0x5A) 

    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,

    "lwindow",       // VK_LWIN           0x5B 
    "rwindow",       // VK_RWIN           0x5C 
    "apps",          // VK_APPS           0x5D 
    NULL,               //    0x5E                
    "sleep",
    "kp-0",          // VK_NUMPAD0        0x60 
    "kp-1",          // VK_NUMPAD1        0x61 
    "kp-2",          // VK_NUMPAD2        0x62 
    "kp-3",          // VK_NUMPAD3        0x63 
    "kp-4",          // VK_NUMPAD4        0x64 
    "kp-5",          // VK_NUMPAD5        0x65 
    "kp-6",          // VK_NUMPAD6        0x66 
    "kp-7",          // VK_NUMPAD7        0x67 
    "kp-8",          // VK_NUMPAD8        0x68 
    "kp-9",          // VK_NUMPAD9        0x69 
    "kp-multiply",   // VK_MULTIPLY       0x6A 
    "kp-add",        // VK_ADD            0x6B 
    "kp-separator",  // VK_SEPARATOR      0x6C 
    "kp-subtract",   // VK_SUBTRACT       0x6D 
    "kp-decimal",    // VK_DECIMAL        0x6E 
    "kp-divide",     // VK_DIVIDE         0x6F 
    "f1",            // VK_F1             0x70 
    "f2",            // VK_F2             0x71 
    "f3",            // VK_F3             0x72 
    "f4",            // VK_F4             0x73 
    "f5",            // VK_F5             0x74 
    "f6",            // VK_F6             0x75 
    "f7",            // VK_F7             0x76 
    "f8",            // VK_F8             0x77 
    "f9",            // VK_F9             0x78 
    "f10",           // VK_F10            0x79 
    "f11",           // VK_F11            0x7A 
    "f12",           // VK_F12            0x7B 
    "f13",           // VK_F13            0x7C 
    "f14",           // VK_F14            0x7D 
    "f15",           // VK_F15            0x7E 
    "f16",           // VK_F16            0x7F 
    "f17",           // VK_F17            0x80 
    "f18",           // VK_F18            0x81 
    "f19",           // VK_F19            0x82 
    "f20",           // VK_F20            0x83 
    "f21",           // VK_F21            0x84 
    "f22",           // VK_F22            0x85 
    "f23",           // VK_F23            0x86 
    "f24",           // VK_F24            0x87 

    NULL, NULL, NULL, NULL,      //    0x88 .. 0x8B        
    NULL, NULL, NULL, NULL,      //    0x8C .. 0x8F        

    "kp-numlock",    // VK_NUMLOCK        0x90 
    "scroll",        // VK_SCROLL         0x91 
    //Not sure where the following block comes from.
    //   Windows headers have NEC and Fujitsu specific keys in
    //   this block, but nothing generic.  
    "kp-space",	     // VK_NUMPAD_CLEAR   0x92 
    "kp-enter",	     // VK_NUMPAD_ENTER   0x93 
    "kp-prior",	     // VK_NUMPAD_PRIOR   0x94 
    "kp-next",	     // VK_NUMPAD_NEXT    0x95 
    "kp-end",	     // VK_NUMPAD_END     0x96 
    "kp-home",	     // VK_NUMPAD_HOME    0x97 
    "kp-left",	     // VK_NUMPAD_LEFT    0x98 
    "kp-up",	     // VK_NUMPAD_UP      0x99 
    "kp-right",	     // VK_NUMPAD_RIGHT   0x9A 
    "kp-down",	     // VK_NUMPAD_DOWN    0x9B 
    "kp-insert",     // VK_NUMPAD_INSERT  0x9C 
    "kp-delete",     // VK_NUMPAD_DELETE  0x9D 

    NULL, NULL,	     //    0x9E .. 0x9F        

    // /* 
    //  * VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
    //  * Used only as parameters to GetAsyncKeyState and GetKeyState.
    //  * No other API or message will distinguish left and right keys this way.
    //  * 0xA0 .. 0xA5
    //  */
    "lshift", 
    "rshift",
    "lcontrol",
    "rcontrol",
    "lmenu",
    "rmenu",

    // /* Multimedia keys. These are handled as WM_APPCOMMAND, which allows us
    //    to enable them selectively, and gives access to a few more functions.
    //    See lispy_multimedia_keys below.  */
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, // 0xA6 .. 0xAC        Browser 
    NULL, NULL, NULL,             // 0xAD .. 0xAF         Volume 
    NULL, NULL, NULL, NULL,          // 0xB0 .. 0xB3          Media 
    NULL, NULL, NULL, NULL,          // 0xB4 .. 0xB7           Apps 

    // 0xB8 .. 0xC0 "OEM" keys - all seem to be punctuation.  
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,

    // 0xC1 - 0xDA unallocated, 0xDB-0xDF more OEM keys 
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,

    NULL,               // 0xE0                   
    "ax",            // VK_OEM_AX         0xE1 
    NULL,               // VK_OEM_102        0xE2 
    "ico-help",      // VK_ICO_HELP       0xE3 
    "ico-00",        // VK_ICO_00         0xE4 
    NULL,               // VK_PROCESSKEY     0xE5 
    "ico-clear",     // VK_ICO_CLEAR      0xE6 
    "packet",        // VK_PACKET         0xE7 
    NULL,               //                   0xE8 
    "reset",         // VK_OEM_RESET      0xE9 
    "jump",          // VK_OEM_JUMP       0xEA 
    "oem-pa1",       // VK_OEM_PA1        0xEB 
    "oem-pa2",       // VK_OEM_PA2        0xEC 
    "oem-pa3",       // VK_OEM_PA3        0xED 
    "wsctrl",        // VK_OEM_WSCTRL     0xEE 
    "cusel",         // VK_OEM_CUSEL      0xEF 
    "oem-attn",      // VK_OEM_ATTN       0xF0 
    "finish",        // VK_OEM_FINISH     0xF1 
    "copy",          // VK_OEM_COPY       0xF2 
    "auto",          // VK_OEM_AUTO       0xF3 
    "enlw",          // VK_OEM_ENLW       0xF4 
    "backtab",       // VK_OEM_BACKTAB    0xF5 
    "attn",          // VK_ATTN           0xF6 
    "crsel",         // VK_CRSEL          0xF7 
    "exsel",         // VK_EXSEL          0xF8 
    "ereof",         // VK_EREOF          0xF9 
    "play",          // VK_PLAY           0xFA 
    "zoom",          // VK_ZOOM           0xFB 
    "noname",        // VK_NONAME         0xFC 
    "pa1",           // VK_PA1            0xFD 
    "oem_clear",     // VK_OEM_CLEAR      0xFE 
    NULL // 0xFF 
};


string get_key_desc(u32 vk, u32 sc, modifier_t mod)
{
	return '';
}
//return false if IME don't want this key
//return true if want it
BOOL WINAPI ImeProcessKey(HIMC /*hIMC*/,
						  u32 vk, LPARAM scan_code,
						  CONST LPBYTE kbd_state)
{

	static bool ime_connected = false;

	if (!ime_connected) {
		BHJDEBUG(" connect_ime_server");
		ime_connected = true;
		connect_ime_server();
	}
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

	if (vk > sizeof(special_keys)/sizeof(special_keys[0])) {
		return false;
	}

	string key_desc = get_key_desc(vk, scan_code, mod);
	if (key_desc.empty()) {
		return false;
	}

	if (want(key_desc)) {
		return true;
	}
	return false;
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
			if (g_quail_rules.empty()) {
				init_quail_rules();
			}
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

				if (index > g_last_cand || index >= cands.size()) {
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
	string C3;
	if (ws.size() > 2) { 
		C3 = to_string(ws.substr(2, 1));
	}
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
	} else if (n == 3) {
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
	} else {
		for (size_t i=0; i < g_reverse_rules[C1].size(); i++) {
			for (size_t j=0; j < g_reverse_rules[C2].size(); j++) {
				for (size_t k=0; k < g_reverse_rules[C3].size(); k++) {
					for (size_t l=0; l < g_reverse_rules[Cn].size(); l++) {
						res_list.push_back(
							g_reverse_rules[C1][i].substr(0, 1) + 
							g_reverse_rules[C2][j].substr(0, 1) +
							g_reverse_rules[C3][k].substr(0, 1) +
							g_reverse_rules[Cn][l].substr(0, 1));
					}
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
