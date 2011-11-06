#include <windows.h>
#include <immdev.h>
#include <imedefs.h>
#include "imewnd.h"
#include <algorithm>
#include "ime-socket.h"
using std::reverse;

typedef unsigned int modifier_t;

const modifier_t mod_none = 0;
const modifier_t mod_ctrl = 1;
const modifier_t mod_shift = 2;
const modifier_t mod_menu = 4;
const modifier_t mod_ctrlshift = mod_ctrl | mod_shift;
const modifier_t mod_ctrlmenu = mod_ctrl | mod_menu;
const modifier_t mod_shiftmenu = mod_shift | mod_menu;
const modifier_t mod_ctrlshiftmenu = mod_ctrl | mod_shift | mod_menu;


static modifier_t get_mod_state ()
{
	modifier_t mod = 0;
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

#define beep() do {								\
		if (getenv("SCIM_DEBUG")) {				\
			BHJDEBUG(" Beep");					\
		}										\
		beep();									\
	} while (0)


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

    "shift",                // VK_SHIFT          0x10 
    "control",                // VK_CONTROL        0x11 
    "menu",                // VK_MENU           0x12 
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
    "space",                // VK_SPACE          0x20 
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


static char get_ascii(u32 vk, u32 sc, modifier_t mod)
{
	BYTE keystate[256];
	memset (keystate, 0, sizeof (keystate));
	keystate[vk] = 0x80;
	if (mod & mod_shift) {
		keystate[VK_SHIFT] = 0x80;
	}

	if (mod & mod_ctrl) {
		keystate[VK_CONTROL] = 0x80;
		keystate[VK_LCONTROL] = 0x80;
	} 
	
	if (mod & mod_menu) {
		keystate[VK_MENU] = 0x80;
		keystate[VK_RMENU] = 0x80;
    }

	unsigned char ascii[2] = "";
	ToAscii(vk, sc, keystate, (LPWORD)ascii, 0);
	return ascii[0];
}

static char dvorak_charmap(char c)
{
    int n = (unsigned char) c;
    static char dvorak[256];
    static int inited = 0;

    if (inited) {
        return dvorak[n];
    }

    for (int i=0; i < sizeof dvorak; i++) {
        dvorak[i] = i; //default map to self
    }

#define map_it(c_querty, c_dvorak) dvorak[(unsigned char) c_querty] = c_dvorak;
    map_it('`', '$'); map_it('\'', '-'); map_it('"', '_');
    map_it('1', '&'); map_it('!', '%'); map_it('2', '[');
    map_it('@', '7'); map_it('3', '{'); map_it('#', '5');
    map_it('4', '}'); map_it('$', '3'); map_it('5', '(');
    map_it('%', '1'); map_it('6', '='); map_it('^', '9');
    map_it('7', '*'); map_it('&', '0'); map_it('8', ')');
    map_it('*', '2'); map_it('9', '+'); map_it('(', '4');
    map_it('0', ']'); map_it(')', '6'); map_it('-', '!');
    map_it('_', '8'); map_it('=', '#'); map_it('+', '`');
    map_it(']', '@'); map_it('}', '^'); map_it('q', ';');
    map_it('w', ','); map_it('e', '.'); map_it('r', 'p');
    map_it('t', 'y'); map_it('y', 'f'); map_it('u', 'g');
    map_it('i', 'c'); map_it('o', 'r'); map_it('p', 'l');
    map_it('[', '/'); map_it('s', 'o'); map_it('d', 'e');
    map_it('f', 'u'); map_it('g', 'i'); map_it('h', 'd');
    map_it('j', 'h'); map_it('k', 't'); map_it('l', 'n');
    map_it(';', 's'); map_it('z', '\''); map_it('x', 'q');
    map_it('c', 'j'); map_it('v', 'k'); map_it('b', 'x');
    map_it('n', 'b'); map_it(',', 'w'); map_it('.', 'v');
    map_it('/', 'z'); map_it('Q', ':'); map_it('W', '<');
    map_it('E', '>'); map_it('R', 'P'); map_it('T', 'Y');
    map_it('Y', 'F'); map_it('U', 'G'); map_it('I', 'C');
    map_it('O', 'R'); map_it('P', 'L'); map_it('{', '?');
    map_it('S', 'O'); map_it('D', 'E'); map_it('F', 'U');
    map_it('G', 'I'); map_it('H', 'D'); map_it('J', 'H');
    map_it('K', 'T'); map_it('L', 'N'); map_it(':', 'S');
    map_it('Z', '"'); map_it('X', 'Q'); map_it('C', 'J');
    map_it('V', 'K'); map_it('B', 'X'); map_it('N', 'B');
    map_it('<', 'W'); map_it('>', 'V'); map_it('?', 'Z');
    
    inited = 1;
    return dvorak[n];
}

static string get_key_desc(u32 vk, u32 sc, modifier_t mod)
{
	if (vk > sizeof(special_keys)/sizeof(special_keys[0])) {
		return "";
	}

	if (special_keys[vk]) {
		if (!strcmp(special_keys[vk], "control") 
			|| !strcmp(special_keys[vk], "lcontrol")
			|| !strcmp(special_keys[vk], "rcontrol")) {
			mod &= ~mod_ctrl;
		} else if (!strcmp(special_keys[vk], "shift") 
				   || !strcmp(special_keys[vk], "lshift")
				   || !strcmp(special_keys[vk], "rshift")) {
			mod &= ~mod_shift;
		} else if (!strcmp(special_keys[vk], "menu") 
				   || !strcmp(special_keys[vk], "lmenu")
				   || !strcmp(special_keys[vk], "rmenu")) {
			mod &= ~mod_menu;
		}
	}

	if (mod == mod_none) {
		if (special_keys[vk]) {
			return special_keys[vk];
		} else {
			char c = get_ascii(vk, sc, mod);
			if (isgraph(c)) {
				string ret;
				ret.push_back(dvorak_charmap(c));
				return ret;
			} else {
				bhjerr(" Error: vk not special, and not graph: %d", vk);
			}
		}
	}

	if (mod == mod_shift) {
		if (special_keys[vk]) {
			return string("S ") + special_keys[vk];
		} else {
			char c = get_ascii(vk, sc, mod);
			if (isgraph(c)) {
				string ret;
				ret.push_back(dvorak_charmap(c));
				return ret;
			} else {
				bhjerr(" Error: shift + vk not special, and not graph: %d", vk);
			}
		}
	}

	if (mod & mod_menu) {
		return string("A ") + get_key_desc(vk, sc, mod & ~mod_menu);
	}

	if (mod & mod_ctrl) {
		return string("C ") + get_key_desc(vk, sc, mod & ~mod_ctrl);
	}
	
	return "";
}
//return false if IME don't want this key
//return true if want it

static bool want(const string& key_desc)
{
	ime_write_line(string_format("want %s?", key_desc.c_str()));
	string ret = ime_recv_line();
	ime_recv_line(); //read the "end:" off 
	if (ret == "yes") {
		return true;
	} else if (ret == "no") {
		return false;
	} else {
		bhjerr(" want: got wrong answer");
	}
}

BOOL WINAPI ImeProcessKey(HIMC /*hIMC*/,
						  u32 vk, LPARAM scan_code,
						  CONST LPBYTE kbd_state)
{

	if (vk == VK_SHIFT || vk == VK_CONTROL || vk == VK_MENU) {
		return false;
	}

    BYTE ks[256];
    GetKeyboardState(ks);
    if (GetAsyncKeyState(VK_SHIFT) & 0x8000) {
        ks[VK_SHIFT] = 129;
    } else {
        ks[VK_SHIFT] = 0;
    }
    SetKeyboardState(ks);

    if (vk < 255) { // always process
        return true;
    }
}

BOOL WINAPI MyImeProcessKey(HIMC /*hIMC*/,
						  u32 vk, LPARAM scan_code,
						  CONST LPBYTE kbd_state)
{

	static bool ime_connected = false;

	if (!ime_connected) {
		ime_connected = true;
		connect_ime_server();
	}
	// BHJDEBUG(" vk is %x, state is %s; scan_code is cnt:%d, sc:0x%02x, ext:%s, prev:%s", 
	// 		 vk, 
    //          kbd_state[vk & 0xff] ? "down" : "up",
	// 		 (scan_code&0xffff),
	// 		 (scan_code & (0xff<<16)) >> 16,
	// 		 scan_code & (1 << 24) ? "yes" : "no", //extended, e.g., right ctrl/shift/menu
	// 		 scan_code & (1<<30) ? "down" : "up");
//#define bhjreturn(ret) BHJDEBUG(" return " #ret); return ret
#define bhjreturn(ret) return ret
	vk = LOWORD(vk);
	modifier_t mod = get_mod_state();

	string key_desc = get_key_desc(vk, scan_code, mod);
	if (key_desc.empty()) {
		return false;
	}

	if (want(key_desc)) {
		return true;
	}
	return false;
}


bool string_begin_with(const string& src, const string& dst)
{
	if (src.size() < dst.size()) {
		return false;
	}

	if (!strncmp(src.c_str(), dst.c_str(), dst.size())) {
		return true;
	}
	return false;
}

class ime_client
{
public:
	string compstr;
	string candsstr;
	string cand_idx;
	string hintstr;
	string activestr;
	string commitstr;
	string beepstr;
};

ime_client get_client_reply()
{
	ime_client client;
	for (;;) {
		string str = ime_recv_line();
		if (str.empty() || str == "end:") {
			break;
		}

		if (string_begin_with(str, "commit: ")) {
			client.commitstr = str.substr(strlen("commit: "));
		} else if (string_begin_with(str, "hint: ")) {
			client.hintstr = str.substr(strlen("hint: "));
		} else if (string_begin_with(str, "comp: ")) {
			client.compstr = str.substr(strlen("comp: "));
		} else if (string_begin_with(str, "cands: ")) {
			client.candsstr = str.substr(strlen("cands: "));
		} else if (string_begin_with(str, "cand_index: ")) {
			client.cand_idx = str.substr(strlen("cand_index: "));
		} else if (string_begin_with(str, "active: ")) {
			client.activestr = str.substr(strlen("active: "));
		} else if (string_begin_with(str, "beep: ")) {
			client.beepstr = str.substr(strlen("beep: "));
		} else {
			client.compstr = str;
			beep();
		}
	}
	return client;
}

bool shift_on(CONST LPBYTE kbd_state) 
{
    return GetAsyncKeyState(VK_SHIFT) & 0x8000;
}

bool caps_on(CONST LPBYTE kbd_state)
{
    
    return kbd_state[VK_CAPITAL] & 0x1;
}

bool shift_caps_on(CONST LPBYTE kbd_state)
{
    return shift_on(kbd_state) ^ caps_on(kbd_state);
}

void wrap_shift_up_down(input_context& ic, u32 vk);
static void wrap_shift_down_up(input_context& ic, u32 vk)
{
    BYTE ks[256];
    GetKeyboardState(ks);
    ks[VK_SHIFT] = 129;
    SetKeyboardState(ks);
    ic.add_msg(WM_IME_KEYDOWN, vk, 1);
}

static void wrap_shift_up_down(input_context& ic, u32 vk)
{
    BYTE ks[256];
    GetKeyboardState(ks);
    ks[VK_SHIFT] = 0;
    SetKeyboardState(ks);
    ic.add_msg(WM_IME_KEYDOWN, vk, 1);
}

static u32 dvorak_handle_key(input_context& ic, u32 vk, u32 scan_code, CONST LPBYTE kbd_state) 
{

    struct dvo_map_desc {
        int q_vk;
        int d_vk;
        int d_vk_uc;
        int d_vk_shift;
        int d_vk_uc_shift;
        bool caps_shift;
    };

#define map_it(q, d) {q, d, 0, 0, 0, 0}
    static struct dvo_map_desc maps[] = {
        map_it('Q', VK_OEM_1), //;:
        map_it('W', VK_OEM_COMMA), //,<
        map_it('E', VK_OEM_PERIOD),
        map_it('R', 'P'),
        map_it('T', 'Y'),
        map_it('Y', 'F'),
        map_it('U', 'G'),
        map_it('I', 'C'),
        map_it('O', 'R'),
        map_it('P', 'L'),
        map_it(VK_OEM_4, VK_OEM_2),
        map_it('S', 'O'),
        map_it('D', 'E'),
        map_it('F', 'U'),
        map_it('G', 'I'),
        map_it('H', 'D'),
        map_it('J', 'H'),
        map_it('K', 'T'),
        map_it('L', 'N'),
        map_it(VK_OEM_1, 'S'), //;:
        map_it('Z', VK_OEM_7), //'"
        map_it('X', 'Q'),
        map_it('C', 'J'),
        map_it('V', 'K'),
        map_it('B', 'X'),
        map_it('N', 'B'),
        map_it(VK_OEM_COMMA, 'W'),
        map_it(VK_OEM_PERIOD, 'V'),
        map_it(VK_OEM_2, 'Z'),
#define map_it_no_caps(q, d, du, ds, dus) {q, d, du, ds, dus, 0}
        map_it_no_caps(VK_OEM_3, '4', VK_OEM_3, 1, 1), //`~ -> $~
        map_it_no_caps(VK_OEM_6, '2', '6', 1, 1), //]} -> @^
        map_it_no_caps(VK_OEM_PLUS, '3', VK_OEM_3, 1, 0), //=+ -> #`
        map_it_no_caps('1', '7', '5', 1, 1),
#define map_it_caps(q, d, du, ds, dus) {q, d, du, ds, dus, 1}
        map_it_caps(VK_OEM_7, VK_OEM_MINUS, VK_OEM_MINUS, 0, 1),
        map_it_caps('2', VK_OEM_4, '7', 0, 0),
        map_it_caps('3', VK_OEM_4, '5', 1, 0),
        map_it_caps('4', VK_OEM_6, '3', 1, 0),
        map_it_caps('5', '9', '1', 1, 0),
        map_it_caps('6', VK_OEM_PLUS, '9', 0, 0),
        map_it_caps('7', '8', '0', 1, 0),
        map_it_caps('8', '0', '2', 1, 0),
        map_it_caps('9', VK_OEM_PLUS, '4', 1, 0),
        map_it_caps('0', VK_OEM_6, '6', 0, 0),
        map_it_caps(VK_OEM_MINUS, '1', '8', 1, 0),
    };

    for (int i = 0; i < sizeof (maps) / sizeof (maps[0]); i++) {
        if (vk == maps[i].q_vk) {
            if (maps[i].d_vk_uc == 0) {
                vk = maps[i].d_vk;
                break;
            }

            if (maps[i].caps_shift == 0) {
                if (shift_on(kbd_state)) {
                    if (maps[i].d_vk_uc_shift) {
                        wrap_shift_down_up(ic, maps[i].d_vk_uc);
                    } else {
                        wrap_shift_up_down(ic, maps[i].d_vk_uc);
                    }
                } else {
                    if (maps[i].d_vk_shift) {
                        wrap_shift_down_up(ic, maps[i].d_vk);
                    } else {
                        wrap_shift_up_down(ic, maps[i].d_vk);
                    }
                }
            } else {
                if (shift_caps_on(kbd_state)) {
                    if (maps[i].d_vk_uc_shift) {
                        wrap_shift_down_up(ic, maps[i].d_vk_uc);
                    } else {
                        wrap_shift_up_down(ic, maps[i].d_vk_uc);
                    }
                } else {
                    if (maps[i].d_vk_shift) {
                        wrap_shift_down_up(ic, maps[i].d_vk);
                    } else {
                        wrap_shift_up_down(ic, maps[i].d_vk);
                    }
                }
            }
            return ic.return_ime_msgs();
        }
    }

    ic.add_msg(WM_IME_KEYDOWN, vk, 1);
    return ic.return_ime_msgs();
}

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

    if (!MyImeProcessKey(hIMC, vk, scan_code, kbd_state)) {
        return dvorak_handle_key(ic, vk, scan_code, kbd_state);
    }
        
	const modifier_t mod = get_mod_state();

	string key_desc = get_key_desc(vk, scan_code, mod);

	ime_write_line(string_format("keyed %s", key_desc.c_str()));
	ime_client client = get_client_reply();
	if (g_ime_name != client.activestr) {
		ic.add_update_status_msg();
		g_ime_name = client.activestr;
	}

	if (g_comp_str.empty() && !client.compstr.empty()) {
		ic.add_msg(WM_IME_STARTCOMPOSITION, 0, 0);
	}

	if (g_comp_str != client.compstr
		|| g_cands_str != client.candsstr
		|| g_cand_idx_str != client.cand_idx
		|| g_hint_str != client.hintstr) {
		g_comp_str = client.compstr;
		g_cands_str = client.candsstr;
		g_cand_idx_str = client.cand_idx;
		g_hint_str = client.hintstr;
		ic.add_show_comp_msg();
	}

	if (!client.commitstr.empty()) {
		ic.send_text(client.commitstr);
	}

	g_hint_str = client.hintstr;
	if (!client.beepstr.empty()) {
		beep();
	}

	return ic.return_ime_msgs();
}


// VK_LBUTTON
// VK_RBUTTON
// VK_CANCEL
// VK_MBUTTON
// VK_XBUTTON1
// VK_XBUTTON2
// VK_BACK
// VK_TAB
// VK_CLEAR
// VK_RETURN
// VK_SHIFT
// VK_CONTROL
// VK_MENU
// VK_PAUSE
// VK_CAPITAL
// VK_KANA
// VK_HANGEUL
// VK_HANGUL
// VK_JUNJA
// VK_FINAL
// VK_HANJA
// VK_KANJI
// VK_ESCAPE
// VK_CONVERT
// VK_NONCONVERT
// VK_ACCEPT
// VK_MODECHANGE
// VK_SPACE
// VK_PRIOR
// VK_NEXT
// VK_END
// VK_HOME
// VK_LEFT
// VK_UP
// VK_RIGHT
// VK_DOWN
// VK_SELECT
// VK_PRINT
// VK_EXECUTE
// VK_SNAPSHOT
// VK_INSERT
// VK_DELETE
// VK_HELP
// VK_0
// VK_A
// VK_LWIN
// VK_RWIN
// VK_APPS
// VK_SLEEP
// VK_NUMPAD0
// VK_NUMPAD1
// VK_NUMPAD2
// VK_NUMPAD3
// VK_NUMPAD4
// VK_NUMPAD5
// VK_NUMPAD6
// VK_NUMPAD7
// VK_NUMPAD8
// VK_NUMPAD9
// VK_MULTIPLY
// VK_ADD
// VK_SEPARATOR
// VK_SUBTRACT
// VK_DECIMAL
// VK_DIVIDE
// VK_F1
// VK_F2
// VK_F3
// VK_F4
// VK_F5
// VK_F6
// VK_F7
// VK_F8
// VK_F9
// VK_F10
// VK_F11
// VK_F12
// VK_F13
// VK_F14
// VK_F15
// VK_F16
// VK_F17
// VK_F18
// VK_F19
// VK_F20
// VK_F21
// VK_F22
// VK_F23
// VK_F24
// VK_NUMLOCK
// VK_SCROLL
// VK_OEM_NEC_EQUAL
// VK_OEM_FJ_JISHO
// VK_OEM_FJ_MASSHOU
// VK_OEM_FJ_TOUROKU
// VK_OEM_FJ_LOYA
// VK_OEM_FJ_ROYA
// VK_L*
// VK_LSHIFT
// VK_RSHIFT
// VK_LCONTROL
// VK_RCONTROL
// VK_LMENU
// VK_RMENU
// VK_BROWSER_BACK
// VK_BROWSER_FORWARD
// VK_BROWSER_REFRESH
// VK_BROWSER_STOP
// VK_BROWSER_SEARCH
// VK_BROWSER_FAVORITES
// VK_BROWSER_HOME
// VK_VOLUME_MUTE
// VK_VOLUME_DOWN
// VK_VOLUME_UP
// VK_MEDIA_NEXT_TRACK
// VK_MEDIA_PREV_TRACK
// VK_MEDIA_STOP
// VK_MEDIA_PLAY_PAUSE
// VK_LAUNCH_MAIL
// VK_LAUNCH_MEDIA_SELECT
// VK_LAUNCH_APP1
// VK_LAUNCH_APP2
// VK_OEM_1
// VK_OEM_PLUS
// VK_OEM_COMMA
// VK_OEM_MINUS
// VK_OEM_PERIOD
// VK_OEM_2
// VK_OEM_3
// VK_OEM_4
// VK_OEM_5
// VK_OEM_6
// VK_OEM_7
// VK_OEM_8
// VK_OEM_AX
// VK_OEM_102
// VK_ICO_HELP
// VK_ICO_00
// VK_PROCESSKEY
// VK_ICO_CLEAR
// VK_PACKET
// VK_OEM_RESET
// VK_OEM_JUMP
// VK_OEM_PA1
// VK_OEM_PA2
// VK_OEM_PA3
// VK_OEM_WSCTRL
// VK_OEM_CUSEL
// VK_OEM_ATTN
// VK_OEM_FINISH
// VK_OEM_COPY
// VK_OEM_AUTO
// VK_OEM_ENLW
// VK_OEM_BACKTAB
// VK_ATTN
// VK_CRSEL
// VK_EXSEL
// VK_EREOF
// VK_PLAY
// VK_ZOOM
// VK_NONAME
// VK_PA1
// VK_OEM_CLEAR
// MAPVK_VK_TO_VSC
// MAPVK_VSC_TO_VK
// MAPVK_VK_TO_CHAR
// MAPVK_VSC_TO_VK_EX
// MAPVK_VK_TO_VSC_EX
