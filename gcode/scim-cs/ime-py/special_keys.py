special_keys = (
    None,                # 0                      

    None,                # VK_LBUTTON        0x01 
    None,                # VK_RBUTTON        0x02 
    "cancel",         # VK_CANCEL         0x03 
    None,                # VK_MBUTTON        0x04 

    None, None, None,          #    0x05 .. 0x07        

    "backspace",      # VK_BACK           0x08 
    "tab",            # VK_TAB            0x09 

    None, None,             #    0x0A .. 0x0B        

    "clear",          # VK_CLEAR          0x0C 
    "return",         # VK_RETURN         0x0D 

    None, None,             #    0x0E .. 0x0F        

    "shift",                # VK_SHIFT          0x10 
    "control",                # VK_CONTROL        0x11 
    "menu",                # VK_MENU           0x12 
    "pause",          # VK_PAUSE          0x13 
    "capslock",       # VK_CAPITAL        0x14 
    "kana",           # VK_KANA/VK_HANGUL 0x15 
    None,                #    0x16                
    "junja",          # VK_JUNJA          0x17 
    "final",          # VK_FINAL          0x18 
    "kanji",          # VK_KANJI/VK_HANJA 0x19 
    None,                #    0x1A                
    "escape",         # VK_ESCAPE         0x1B 
    "convert",        # VK_CONVERT        0x1C 
    "non-convert",    # VK_NONCONVERT     0x1D 
    "accept",         # VK_ACCEPT         0x1E 
    "mode-change",    # VK_MODECHANGE     0x1F 
    'space',                # VK_SPACE          0x20 
    "prior",          # VK_PRIOR          0x21 
    "next",           # VK_NEXT           0x22 
    "end",            # VK_END            0x23 
    "home",           # VK_HOME           0x24 
    "left",           # VK_LEFT           0x25 
    "up",             # VK_UP             0x26 
    "right",          # VK_RIGHT          0x27 
    "down",           # VK_DOWN           0x28 
    "select",         # VK_SELECT         0x29 
    "print",          # VK_PRINT          0x2A 
    "execute",        # VK_EXECUTE        0x2B 
    "snapshot",       # VK_SNAPSHOT       0x2C 
    "insert",         # VK_INSERT         0x2D 
    "delete",         # VK_DELETE         0x2E 
    "help",           # VK_HELP           0x2F 

    # VK_0 thru VK_9 are the same as ASCII '0' thru '9' (0x30 - 0x39) 

    None, None, None, None, None, None, None, None, None, None,

    None, None, None, None, None, None, None, # 0x3A .. 0x40       

    # VK_A thru VK_Z are the same as ASCII 'A' thru 'Z' (0x41 - 0x5A) 

    None, None, None, None, None, None, None, None, None,
    None, None, None, None, None, None, None, None, None,
    None, None, None, None, None, None, None, None,

    "lwindow",       # VK_LWIN           0x5B 
    "rwindow",       # VK_RWIN           0x5C 
    "apps",          # VK_APPS           0x5D 
    None,               #    0x5E                
    "sleep",
    "kp-0",          # VK_NUMPAD0        0x60 
    "kp-1",          # VK_NUMPAD1        0x61 
    "kp-2",          # VK_NUMPAD2        0x62 
    "kp-3",          # VK_NUMPAD3        0x63 
    "kp-4",          # VK_NUMPAD4        0x64 
    "kp-5",          # VK_NUMPAD5        0x65 
    "kp-6",          # VK_NUMPAD6        0x66 
    "kp-7",          # VK_NUMPAD7        0x67 
    "kp-8",          # VK_NUMPAD8        0x68 
    "kp-9",          # VK_NUMPAD9        0x69 
    "kp-multiply",   # VK_MULTIPLY       0x6A 
    "kp-add",        # VK_ADD            0x6B 
    "kp-separator",  # VK_SEPARATOR      0x6C 
    "kp-subtract",   # VK_SUBTRACT       0x6D 
    "kp-decimal",    # VK_DECIMAL        0x6E 
    "kp-divide",     # VK_DIVIDE         0x6F 
    "f1",            # VK_F1             0x70 
    "f2",            # VK_F2             0x71 
    "f3",            # VK_F3             0x72 
    "f4",            # VK_F4             0x73 
    "f5",            # VK_F5             0x74 
    "f6",            # VK_F6             0x75 
    "f7",            # VK_F7             0x76 
    "f8",            # VK_F8             0x77 
    "f9",            # VK_F9             0x78 
    "f10",           # VK_F10            0x79 
    "f11",           # VK_F11            0x7A 
    "f12",           # VK_F12            0x7B 
    "f13",           # VK_F13            0x7C 
    "f14",           # VK_F14            0x7D 
    "f15",           # VK_F15            0x7E 
    "f16",           # VK_F16            0x7F 
    "f17",           # VK_F17            0x80 
    "f18",           # VK_F18            0x81 
    "f19",           # VK_F19            0x82 
    "f20",           # VK_F20            0x83 
    "f21",           # VK_F21            0x84 
    "f22",           # VK_F22            0x85 
    "f23",           # VK_F23            0x86 
    "f24",           # VK_F24            0x87 

    None, None, None, None,      #    0x88 .. 0x8B        
    None, None, None, None,      #    0x8C .. 0x8F        

    "kp-numlock",    # VK_NUMLOCK        0x90 
    "scroll",        # VK_SCROLL         0x91 
    #Not sure where the following block comes from.
    #   Windows headers have NEC and Fujitsu specific keys in
    #   this block, but nothing generic.  
    "kp-space",	     # VK_NUMPAD_CLEAR   0x92 
    "kp-enter",	     # VK_NUMPAD_ENTER   0x93 
    "kp-prior",	     # VK_NUMPAD_PRIOR   0x94 
    "kp-next",	     # VK_NUMPAD_NEXT    0x95 
    "kp-end",	     # VK_NUMPAD_END     0x96 
    "kp-home",	     # VK_NUMPAD_HOME    0x97 
    "kp-left",	     # VK_NUMPAD_LEFT    0x98 
    "kp-up",	     # VK_NUMPAD_UP      0x99 
    "kp-right",	     # VK_NUMPAD_RIGHT   0x9A 
    "kp-down",	     # VK_NUMPAD_DOWN    0x9B 
    "kp-insert",     # VK_NUMPAD_INSERT  0x9C 
    "kp-delete",     # VK_NUMPAD_DELETE  0x9D 

    None, None,	     #    0x9E .. 0x9F        

    # /* 
    #  * VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
    #  * Used only as parameters to GetAsyncKeyState and GetKeyState.
    #  * No other API or message will distinguish left and right keys this way.
    #  * 0xA0 .. 0xA5
    #  */
    "lshift", 
    "rshift",
    "lcontrol",
    "rcontrol",
    "lmenu",
    "rmenu",

    # /* Multimedia keys. These are handled as WM_APPCOMMAND, which allows us
    #    to enable them selectively, and gives access to a few more functions.
    #    See lispy_multimedia_keys below.  */
    None, None, None, None, None, None, None, # 0xA6 .. 0xAC        Browser 
    None, None, None,             # 0xAD .. 0xAF         Volume 
    None, None, None, None,          # 0xB0 .. 0xB3          Media 
    None, None, None, None,          # 0xB4 .. 0xB7           Apps 

    # 0xB8 .. 0xC0 "OEM" keys - all seem to be punctuation.  
    None, None, None, None, None, None, None, None, None,

    # 0xC1 - 0xDA unallocated, 0xDB-0xDF more OEM keys 
    None, None, None, None, None, None, None, None, None, None, None, None, None, None, None,
    None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None,

    None,               # 0xE0                   
    "ax",            # VK_OEM_AX         0xE1 
    None,               # VK_OEM_102        0xE2 
    "ico-help",      # VK_ICO_HELP       0xE3 
    "ico-00",        # VK_ICO_00         0xE4 
    None,               # VK_PROCESSKEY     0xE5 
    "ico-clear",     # VK_ICO_CLEAR      0xE6 
    "packet",        # VK_PACKET         0xE7 
    None,               #                   0xE8 
    "reset",         # VK_OEM_RESET      0xE9 
    "jump",          # VK_OEM_JUMP       0xEA 
    "oem-pa1",       # VK_OEM_PA1        0xEB 
    "oem-pa2",       # VK_OEM_PA2        0xEC 
    "oem-pa3",       # VK_OEM_PA3        0xED 
    "wsctrl",        # VK_OEM_WSCTRL     0xEE 
    "cusel",         # VK_OEM_CUSEL      0xEF 
    "oem-attn",      # VK_OEM_ATTN       0xF0 
    "finish",        # VK_OEM_FINISH     0xF1 
    "copy",          # VK_OEM_COPY       0xF2 
    "auto",          # VK_OEM_AUTO       0xF3 
    "enlw",          # VK_OEM_ENLW       0xF4 
    "backtab",       # VK_OEM_BACKTAB    0xF5 
    "attn",          # VK_ATTN           0xF6 
    "crsel",         # VK_CRSEL          0xF7 
    "exsel",         # VK_EXSEL          0xF8 
    "ereof",         # VK_EREOF          0xF9 
    "play",          # VK_PLAY           0xFA 
    "zoom",          # VK_ZOOM           0xFB 
    "noname",        # VK_NONAME         0xFC 
    "pa1",           # VK_PA1            0xFD 
    "oem_clear",     # VK_OEM_CLEAR      0xFE 
    None # 0xFF 
    )
