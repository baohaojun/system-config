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

static string get_key_desc(u32 vk, u32 sc, modifier_t mod)
{
	if (vk >= sizeof(special_keys)/sizeof(special_keys[0])) { // on thinkpad T420, the Fn key will generate a vk=255.
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
				ret.push_back(c);
				return ret;
			} else {
                return "";
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
				ret.push_back(c);
				return ret;
			} else {
				return "";
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

	static bool ime_connected = false;

	if (!ime_connected) {
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
