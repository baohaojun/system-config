/*
Copyright (c) 1997-2010 Roland Kaufmann. All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must contain the above copyright notice.

3. Neither the name "Programmer Dvorak" nor the name "Roland Kaufmann" may be used
   to endorse or promote products derived from this software without specific prior
   written permission.

   For written permission, please contact roland@kaufmann.no. This email address
   may not be used for any other purpose than direct communication with the author.

4. Products derived from this software may not be called "Programmer Dvorak", nor
   may "Programmer Dvorak" appear in their name, without prior written permission
   by Roland Kaufmann.

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL ROLAND KAUFMANN 
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "kbd.h"                // DDK component

// compiler directives to make it work on Win64 as well as on Win32
#pragma setlocale("C")
#if defined(_M_IA64)
#pragma section(".data")
#define LDATA __declspec(allocate(".data"))
#else
#pragma data_seg(".data")
#define LDATA
#endif

// first element is the key that (de)activates the modifier bit
// second element is the bitmask that is toggled
// extra modifiers could be added by mapping a key to VK_KANA and then
// inserting the KBDKANA modifier in the table below
// AltGr is done through Ctrl+Alt translation, so don't enter it here
// array is null terminated since it could be of variable length
static LDATA VK_TO_BIT modifierBits[] = {
        { VK_SHIFT,     KBDSHIFT        },
        { VK_CONTROL,   KBDCTRL         },
        { VK_MENU,      KBDALT          },
        { 0,            0               }
};

static LDATA MODIFIERS modifiers = {
        &modifierBits[ 0 ],
        7,                                      // maximum modifier bitmask
        // all possible combinations of modifier bits are enumerated,
        // stating their modifier state or SHFT_INVALID if there is no
        // map for that state
        // the state number is the column number (+2) in the character
        // translation table
        {
                0,              // 000 base case
                1,              // 001 Shift pressed
                4,              // 010 Ctrl pressed
                5,              // 011 Shift + Ctrl pressed
                SHFT_INVALID,   // 100 Alt pressed
                SHFT_INVALID,   // 101 Shift + Alt pressed
                2,              // 110 Ctrl + Alt pressed -> AltGr
                3               // 111 Shift + Ctrl + Alt pressed
        }
};

// if the key is specified as WCH_NONE, then it is swallowed.
// if WCH_DEAD is used, then the next entry contains 'dead' in
// the first column (as defined below) and then the character
// code for the dead key in the same column(s) that contained
// WCH_DEAD.
#define dead    0xFF

// if this character code is returned (from the dead key table), then no key is
// generated. returning WCH_NONE will not work.
#define cancel  0x0000

// keys that are not displayed but just used as a step in a state
// machine. note that the codes for these keys are those of control
// characters which can not be typed directly, but must be input
// using the Control modifier anyway. don't use 0x0003 as that is
// Ctrl-C, usually Cancel!
#define greek   0x0001
#define math    0x0002
#define compose 0x0004

// control characters
#define escape  0x001B

// this table contains the character definitions for all the
// keys that maps onto two states. the first field is the virtual
// keycode for the key, the second column indicates whether CAPS
// LOCK modifies this key like Shift (this is done by setting the
// field to CAPLOK). the next fields are the character that is
// emitted when the keyboard is in the corresponding state
// the array is null-terminated
// note: the key is only stored in one table; the number of total
// shift states this key can have determines the table
static LDATA VK_TO_WCHARS2 normalAndShift[] = {
        //                                      Normal          Shift
        //----------------------------------------------------
        {       VK_OEM_4,       0,              '&',            '%'             },
        {       VK_'0',         CAPLOK,         '*',            '0',            },
        {       VK_'4',         CAPLOK,         '+',            '4',            },
        //--------------------------------------------------------
        {       VK_TAB,         0,              '\t',           '\t'            },
        {       VK_'F',         CAPLOK,         'f',            'F'             },
        {       VK_'L',         CAPLOK,         'l',            'L'             },
        //--------------------------------------------------------
        {       VK_'I',         CAPLOK,         'i',            'I'             },
        //--------------------------------------------------------
        {       VK_OEM_102,     CAPLOK,         WCH_DEAD,       WCH_DEAD        },
        {       dead,           0,              compose,        compose         },
        {       VK_'Q',         CAPLOK,         'q',            'Q'             },
        {       VK_'J',         CAPLOK,         'j',            'J'             },
        {       VK_'K',         CAPLOK,         'k',            'K'             },
        {       VK_'X',         CAPLOK,         'x',            'X'             },
        {       VK_'B',         CAPLOK,         'b',            'B'             },
        {       VK_'W',         CAPLOK,         'w',            'W'             },
        {       VK_'V',         CAPLOK,         'v',            'V'             },
        {       VK_'Z',         CAPLOK,         'z',            'Z'             },
        //--------------------------------------------------------
        {       0,              0,              '\0',           '\0'            }
};

// this is the keys that have alt-graph combinations
static LDATA VK_TO_WCHARS3 altGrOnly[] = {
        //                                      Normal          Shift           AltGr
        {       VK_OEM_3,       0,              '$',            '~',            WCH_DEAD        },
        {       dead,           0,              WCH_NONE,       WCH_NONE,       '~'             },
        {       VK_'5',         CAPLOK,         '{',            '5',            0x00A2          },
        {       VK_'3',         CAPLOK,         '}',            '3',            0x00A5          },
        {       VK_'1',         CAPLOK,         '(',            '1',            0x20AC          },
        {       VK_'9',         CAPLOK,         '=',            '9',            0x00A3          },
        {       VK_'2',         CAPLOK,         ')',            '2',            0x00BD          },
        {       VK_'8',         CAPLOK,         '!',            '8',            0x00A1          },
        {       VK_OEM_8,       0,              '#',            '`',            WCH_DEAD        },
        {       dead,           0,              WCH_NONE,       WCH_NONE,       '`'             },
        //--------------------------------------------------------------------
        {       VK_OEM_1,       0,              ';',            ':',            WCH_DEAD        },
        {       dead,           0,              WCH_NONE,       WCH_NONE,       0x00A8          },
        {       VK_'G',         CAPLOK,         'g',            'G',            WCH_DEAD        },
        {       dead,           0,              WCH_NONE,       WCH_NONE,       greek           },
        {       VK_OEM_2,       0,              '/',            '?',            0x00BF          },
        //--------------------------------------------------------------------
        {       VK_'H',         CAPLOK,         'h',            'H',            WCH_DEAD        },
        {       dead,           0,              WCH_NONE,       WCH_NONE,       0x00B4          },
        {       VK_'S',         CAPLOK,         's',            'S',            0x00DF          },
        {       VK_OEM_MINUS,   CAPLOK,         '-',            '_',            0x00AD          },
        //--------------------------------------------------------------------
        {       VK_OEM_7,       0,              '\'',           '\"',           WCH_DEAD        },
        {       dead,           0,              WCH_NONE,       WCH_NONE,       0x00B4          },
        {       VK_'M',         CAPLOK,         'm',            'M',            WCH_DEAD        },
        {       dead,           0,              WCH_NONE,       WCH_NONE,       math            },
        //--------------------------------------------------------------------
        {       0,              0,              '\0',           '\0',           '\0'            }
};

// these characters have a capital alt-graph key also.
static LDATA VK_TO_WCHARS4 shiftAltGr[] = {
        //                                              Normal          Shift           AltGr           Shift-AltGr
        {       VK_OEM_COMMA,   0,                      ',',            '<',            0x00AB,         0x201C          },
        {       VK_OEM_PERIOD,  0,                      '.',            '>',            0x00BB,         0x201D          },
        {       VK_'P',         CAPLOK,                 'p',            'P',            0x00B6,         0x00A7          },
        {       VK_'Y',         CAPLOK | CAPLOKALTGR,   'y',            'Y',            0x00FC,         0x00DC          },
        {       VK_'C',         CAPLOK | CAPLOKALTGR,   'c',            'C',            0x00E7,         0x00C7          },
        {       VK_'R',         CAPLOK,                 'r',            'R',            0x00AE,         0x2122          },
        {       VK_OEM_6,       0,                      '@',            '^',            WCH_DEAD,       WCH_DEAD        },
        {       dead,           0,                      WCH_NONE,       WCH_NONE,       '^',            0x02C7          },
        //---------------------------------------------------------------------------------------------------------------
        {       VK_'A',         CAPLOK | CAPLOKALTGR,   'a',            'A',            0x00E5,         0x00C5          },
        {       VK_'O',         CAPLOK | CAPLOKALTGR,   'o',            'O',            0x00F8,         0x00D8          },
        {       VK_'E',         CAPLOK | CAPLOKALTGR,   'e',            'E',            0x00E6,         0x00C6          },
        {       VK_'U',         CAPLOK | CAPLOKALTGR,   'u',            'U',            0x00E9,         0x00C9          },
        {       VK_'D',         CAPLOK | CAPLOKALTGR,   'd',            'D',            0x00F0,         0x00D0          },
        {       VK_'T',         CAPLOK | CAPLOKALTGR,   't',            'T',            0x00FE,         0x00DE          },
        {       VK_'N',         CAPLOK | CAPLOKALTGR,   'n',            'N',            0x00F1,         0x00D1          },
        //---------------------------------------------------------------------------------------------------------------
        {       0,              0,                      '\0',           '\0',           '\0',           '\0'            }
};

// normally, Ctrl makes a control characters; these are the
// exceptions. these keys should rather create some standard
// escape sequences
static LDATA VK_TO_WCHARS5 ctrlKeys[] = {
        //                                      Normal  Shift   AltGr           S-AltGr         Ctrl
        //--------------------------------------------------------------------------------
        {       VK_'7',         CAPLOK,         '[',    '7',    0x00A4,         WCH_NONE,       0x001B          },
        {       VK_'6',         CAPLOK,         ']',    '6',    WCH_NONE,       WCH_NONE,       0x001D          },
        {       VK_OEM_5,       0,              '\\',   '|',    WCH_NONE,       WCH_NONE,       0x001C          },
        {       VK_BACK,        0,              '\b',   '\b',   WCH_NONE,       WCH_NONE,       0x007F          },
        {       VK_ESCAPE,      0,              escape, escape, WCH_NONE,       WCH_NONE,       escape          },
        {       VK_RETURN,      0,              '\r',   '\r',   WCH_NONE,       WCH_NONE,       '\n'            },
        {       VK_SPACE,       0,              ' ',    ' ',    WCH_NONE,       WCH_NONE,       ' '             },
        {       VK_CANCEL,      0,              0x0003, 0x0003, WCH_NONE,       WCH_NONE,       0x0003          },
        //--------------------------------------------------------------------------------
        {       0,              0,              '\0',   '\0',   '\0',           '\0',           '\0'            }
};

static LDATA VK_TO_WCHARS6 shiftCtrl[] = {
        //                                      Normal  Shift   AltGr           S-AltGr         Ctrl            S-Ctrl
        //---------------------------------------------------------------------------------------------------------------
        {       0,              0,              '\0',   '\0',   '\0',           '\0',           '\0',           '\0'    }
};

// hexadecimal characters are available at alt positions. they
// are written is capital, whereas the x is written in small as
// is customary. 456 is considered the home row of the numpad. by
// also putting the equal and dollar signs there, it can also be 
// used in small spreadsheets where the column numbers are low! 
// comma is available as an item separator (in code), colon for 
// entering times and ranges (in spreadsheets).
// I would have preferred to have the hex chars in shift position,
// but Windows automatically cancels NumLock when Shift is pressed.
// decimal won't be available if AltGr is not specified since 
// Ctrl-Alt-Del is intercepted on Windows anyhow!
// note Windows regards the cursor keys as the main keys and then
// does its own translation outside of the scancode to virtual key
// mapping table. there is a hint in the kbd.h header file that
// there exists internally a table called aVkToPfnOem that contains
// the mapping that is done if a key is marked as KBDSPECIAL.
static LDATA VK_TO_WCHARS3 numPad[] = {
        //                      Normal  Shift   AltGr
        //-----------------------------------------------
        { VK_DIVIDE,    0,      '/',    '(',    '('             },
        { VK_MULTIPLY,  0,      '*',    ')',    ')'             },
        { VK_SUBTRACT,  0,      '-',    '$',    '$'             },
        //-----------------------------------------------
#ifdef NO_ATM
        { VK_NUMPAD7,   0,      '1',    'a',    'A'             },
        { VK_NUMPAD8,   0,      '2',    'b',    'B'             },
        { VK_NUMPAD9,   0,      '3',    'c',    'C'             },
        //-----------------------------------------------
        { VK_NUMPAD4,   0,      '4',    'd',    'D'             },
        { VK_NUMPAD5,   0,      '5',    'e',    'E'             },
        { VK_NUMPAD6,   0,      '6',    'f',    'F'             },
        { VK_ADD,       0,      '+',    ',',    ','             },
        //-----------------------------------------------
        { VK_NUMPAD1,   0,      '7',    '=',    '='             },
        { VK_NUMPAD2,   0,      '8',    'x',    'x'             },
        { VK_NUMPAD3,   0,      '9',    ':',    ':'             },
#else
        { VK_NUMPAD1,   0,      '7',    'a',    'A'             },
        { VK_NUMPAD2,   0,      '8',    'b',    'B'             },
        { VK_NUMPAD3,   0,      '9',    'c',    'C'             },
        //-----------------------------------------------
        { VK_NUMPAD4,   0,      '4',    'd',    'D'             },
        { VK_NUMPAD5,   0,      '5',    'e',    'E'             },
        { VK_NUMPAD6,   0,      '6',    'f',    'F'             },
        { VK_ADD,       0,      '+',    ',',    ','             },
        //-----------------------------------------------
        { VK_NUMPAD7,   0,      '1',    '=',    '='             },
        { VK_NUMPAD8,   0,      '2',    'x',    'x'             },
        { VK_NUMPAD9,   0,      '3',    ':',    ':'             },
#endif
        //-----------------------------------------------
        { VK_NUMPAD0,   0,      '0',    '0',    '\\'            },
        { VK_DECIMAL,   0,      '.',    ';',    WCH_NONE        },
        //-----------------------------------------------
        { 0,            0,      '\0',   '\0',   '\0'            }
};

// master table of all translations:
// first element is a pointer to the table of character translations,
// second element is the number of states that is translated
static LDATA VK_TO_WCHAR_TABLE charTranslations[] = {
        { ( PVK_TO_WCHARS1 ) normalAndShift,    2, sizeof( normalAndShift[ 0 ] )        },
        { ( PVK_TO_WCHARS1 ) altGrOnly,         3, sizeof( altGrOnly[ 0 ] )             },
        { ( PVK_TO_WCHARS1 ) shiftAltGr,        4, sizeof( shiftAltGr[ 0 ] )            },
        { ( PVK_TO_WCHARS1 ) ctrlKeys,          5, sizeof( ctrlKeys[ 0 ] )              },
        { ( PVK_TO_WCHARS1 ) shiftCtrl,         6, sizeof( shiftCtrl[ 0 ] )             },
        // put this last so that calls to VkKeyScan() gives characters from the
        // main section before the numeric section when passed a number
        { ( PVK_TO_WCHARS1 ) numPad,            3, sizeof( numPad[ 0 ] )        },
        //----------------------------------------------------------------------------
        { NULL,                                 0, 0                                    }
};

#define none    0xFF

// interpretation of the keyboard input: 
// scanCodesToVirtualKeys[ scanCode ] = virtualKey
// if you want to switch the Ctrl and Caps Lock keys, switch VK_LCONTROL
// (0x1D) and VK_CAPITAL (0x3A)
// these must be flagged with either KBDEXT (extended key), KBDMULTIVK
// (NumLock or Pause), KBDNUMPAD (numeric) or KBDSPECIAL (special processing).
// for compatibility reasons with the original XT keyboard, Ctrl+NumLock is 
// the same as Pause, and Ctrl+ScrollLock is the same as Break (VK_CANCEL)
// see <http://blogs.msdn.com/oldnewthing/archive/2008/02/11/7596539.aspx>
static LDATA USHORT scanCodesToVirtualKeys[] = {
        /* 0x00 */ none,
        /* 0x01 */ VK_ESCAPE,           
        /* 0x02 */ VK_OEM_4,
        /* 0x03 */ VK_'7',
        /* 0x04 */ VK_'5',
        /* 0x05 */ VK_'3',
        /* 0x06 */ VK_'1',
        /* 0x07 */ VK_'9',
        /* 0x08 */ VK_'0',
        /* 0x09 */ VK_'2',
        /* 0x0A */ VK_'4',
        /* 0x0B */ VK_'6',
        /* 0x0C */ VK_'8',
        /* 0x0D */ VK_OEM_8,
        /* 0x0E */ VK_BACK,
        /* 0x0F */ VK_TAB,
        /* 0x10 */ VK_OEM_1,
        /* 0x11 */ VK_OEM_COMMA,
        /* 0x12 */ VK_OEM_PERIOD,
        /* 0x13 */ VK_'P',
        /* 0x14 */ VK_'Y',
        /* 0x15 */ VK_'F',
        /* 0x16 */ VK_'G',
        /* 0x17 */ VK_'C',
        /* 0x18 */ VK_'R',
        /* 0x19 */ VK_'L',
        /* 0x1A */ VK_OEM_2,
        /* 0x1B */ VK_OEM_6,
        /* 0x1C */ VK_RETURN,
        /* 0x1D */ VK_LCONTROL,
        /* 0x1E */ VK_'A',
        /* 0x1F */ VK_'O',
        /* 0x20 */ VK_'E',
        /* 0x21 */ VK_'U',
        /* 0x22 */ VK_'I',
        /* 0x23 */ VK_'D',
        /* 0x24 */ VK_'H',
        /* 0x25 */ VK_'T',
        /* 0x26 */ VK_'N',
        /* 0x27 */ VK_'S',
        /* 0x28 */ VK_OEM_MINUS,
        /* 0x29 */ VK_OEM_3,
        /* 0x2A */ VK_LSHIFT,
        /* 0x2B */ VK_OEM_5,
        /* 0x2C */ VK_OEM_7,
        /* 0x2D */ VK_'Q',
        /* 0x2E */ VK_'J',
        /* 0x2F */ VK_'K',
        /* 0x30 */ VK_'X',
        /* 0x31 */ VK_'B',
        /* 0x32 */ VK_'M',
        /* 0x33 */ VK_'W',
        /* 0x34 */ VK_'V',
        /* 0x35 */ VK_'Z',
        /* 0x36 */ VK_RSHIFT    | KBDEXT,
        /* 0x37 */ VK_MULTIPLY,
        /* 0x38 */ VK_LMENU,
        /* 0x39 */ VK_SPACE,
        /* 0x3A */ VK_CAPITAL,
        /* 0x3B */ VK_F1,
        /* 0x3C */ VK_F2,
        /* 0x3D */ VK_F3,
        /* 0x3E */ VK_F4,
        /* 0x3F */ VK_F5,
        /* 0x40 */ VK_F6,
        /* 0x41 */ VK_F7,
        /* 0x42 */ VK_F8,
        /* 0x43 */ VK_F9,
        /* 0x44 */ VK_F10,
        /* 0x45 */ VK_NUMLOCK   | KBDEXT        | KBDMULTIVK,
        /* 0x46 */ VK_SCROLL    | KBDMULTIVK,
        // apparently the keys at the numeric keypad should NOT be mapped to the
        // VK_NUMPAD? codes but rather to the corresponding navigation key, if
        // you don't want to loose the navigation capabilities of the numpad.
        // (they shouldn't have any of the flags applied to them in that case).
        // Remote Desktop does a reverse lookup through the table to work out the
        // scan code to send; there used to be an issue where it picked the numpad
        // variant making the cursor keys unusable, but this have been fixed.
        // KBDNUMPAD flag indicates that it should be translated to the appropriate
        // VK_NUMPAD? code if shift is pressed, through the ausNumpadCvt table.
        // NumLock acts as a shift lock for the numpad alone (like CapsLock for the
        // main section). KBDSPECIAL must be put on these keys to tell Windows to
        // use the internal translation tables (kbd.h hints that it is called
        // aVkToPfnOem[]).
        // there doesn't seem to be a way to turn off the relationship between
        // Shift and NumLock for the numpad, like there is for the main section,
        // since it is the virtual key codes, not the characters that are of
        // mostly of interest by the applications.
        // if you switch around on the navigation keys, the numeric keys follows,
        // i.e. the translation is done on VK_HOME | KBDNUMPAD => VK_NUMPAD1,
        // not on the 0x47 scan code. note however that Alt+NumPad to enter the
        // ASCII code of a character ALWAYS uses the scan code, no matter the
        // assignment here.
#ifdef NO_ATM
        /* 0x47 */ VK_HOME      /* VK_NUMPAD7 */        | KBDNUMPAD     | KBDSPECIAL,
        /* 0x48 */ VK_UP        /* VK_NUMPAD8 */        | KBDNUMPAD     | KBDSPECIAL,
        /* 0x49 */ VK_PRIOR     /* VK_NUMPAD9 */        | KBDNUMPAD     | KBDSPECIAL,
        /* 0x4A */ VK_SUBTRACT,
        /* 0x4B */ VK_LEFT      /* VK_NUMPAD4 */        | KBDNUMPAD     | KBDSPECIAL,
        /* 0x4C */ VK_CLEAR     /* VK_NUMPAD5 */        | KBDNUMPAD     | KBDSPECIAL,
        /* 0x4D */ VK_RIGHT     /* VK_NUMPAD6 */        | KBDNUMPAD     | KBDSPECIAL,
        /* 0x4E */ VK_ADD,
        /* 0x4F */ VK_END       /* VK_NUMPAD1 */        | KBDNUMPAD     | KBDSPECIAL,
        /* 0x50 */ VK_DOWN      /* VK_NUMPAD2 */        | KBDNUMPAD     | KBDSPECIAL,
        /* 0x51 */ VK_NEXT      /* VK_NUMPAD3 */        | KBDNUMPAD     | KBDSPECIAL,
        /* 0x52 */ VK_INSERT    /* VK_NUMPAD0 */        | KBDNUMPAD     | KBDSPECIAL,
        /* 0x53 */ VK_DELETE    /* VK_DECIMAL */        | KBDNUMPAD     | KBDSPECIAL,
#else
        // when these codes are assigned directly VK_NUMPAD? then NumLock
        // has no effect. however, I recommend that you leave it on since
        // some applications test its state.
        /* 0x47 */ VK_NUMPAD1,
        /* 0x48 */ VK_NUMPAD2,
        /* 0x49 */ VK_NUMPAD3,
        /* 0x4A */ VK_SUBTRACT,
        /* 0x4B */ VK_NUMPAD4,
        /* 0x4C */ VK_NUMPAD5,
        /* 0x4D */ VK_NUMPAD6,
        /* 0x4E */ VK_ADD,
        /* 0x4F */ VK_NUMPAD7,
        /* 0x50 */ VK_NUMPAD8,
        /* 0x51 */ VK_NUMPAD9,
        /* 0x52 */ VK_NUMPAD0,
        /* 0x53 */ VK_DECIMAL,
#endif
        /* 0x54 */ VK_SNAPSHOT  | KBDEXT,
        /* 0x55 */ none,
        /* 0x56 */ VK_OEM_102,
        /* 0x57 */ VK_F11,
        /* 0x58 */ VK_F12
};

// PC keyboards have three kinds of keys: regular scankeys,
// those preceeded by 0xE0 and those preceeded by 0xE1. Windows
// uses just one set of logical "virtual keys".
// first element in this array is the scancode that follows
// 0xE0, second element is the virtual key in which this results
// since the table is of variable length, it must be zero
// terminated. all of these keys have the KBDEXT flag, which seems
// to be necessary for Remote Desktop to see them
static LDATA VSC_VK e0prefixToVirtualKeys[] = {
        { 0x1C, VK_RETURN       | KBDEXT        },
        { 0x1D, VK_RCONTROL     | KBDEXT        },
        { 0x35, VK_DIVIDE       | KBDEXT        },
        { 0x37, VK_SNAPSHOT     | KBDEXT        },
        { 0x38, VK_RMENU        | KBDEXT        },
        { 0x46, VK_CANCEL       | KBDEXT        },
        // so-called "gray" navigation keys; these differ from the ones
        // at the numpad by having the extended prefix 0xE0 added, see:
        // <http://www.win.tue.nl/~aeb/linux/kbd/scancodes-1.html#ss1.5>
        { 0x47, VK_HOME         | KBDEXT        },
        { 0x48, VK_UP           | KBDEXT        },
        { 0x49, VK_PRIOR        | KBDEXT        },
        { 0x4B, VK_LEFT         | KBDEXT        },
        { 0x4D, VK_RIGHT        | KBDEXT        },
        { 0x4F, VK_END          | KBDEXT        },
        { 0x50, VK_DOWN         | KBDEXT        },
        { 0x51, VK_NEXT         | KBDEXT        },
        { 0x52, VK_INSERT       | KBDEXT        },
        { 0x53, VK_DELETE       | KBDEXT        },
        { 0x5B, VK_LWIN         | KBDEXT        },
        { 0x5C, VK_RWIN         | KBDEXT        },
        { 0x5D, VK_APPS         | KBDEXT        },
#if(_WIN32_WINNT >= 0x0500)
        { 0x10, VK_MEDIA_PREV_TRACK     | KBDEXT        },
        { 0x19, VK_MEDIA_NEXT_TRACK     | KBDEXT        },
        { 0x20, VK_VOLUME_MUTE          | KBDEXT        },
        { 0x21, VK_LAUNCH_APP2          | KBDEXT        },
        { 0x22, VK_MEDIA_PLAY_PAUSE     | KBDEXT        },
        { 0x24, VK_MEDIA_STOP           | KBDEXT        },
        { 0x2E, VK_VOLUME_DOWN          | KBDEXT        },
        { 0x30, VK_VOLUME_UP            | KBDEXT        },
        { 0x32, VK_BROWSER_HOME         | KBDEXT        },
        { 0x5F, VK_SLEEP                | KBDEXT        },
        { 0x65, VK_BROWSER_SEARCH       | KBDEXT        },
        { 0x66, VK_BROWSER_FAVORITES    | KBDEXT        },
        { 0x67, VK_BROWSER_REFRESH      | KBDEXT        },
        { 0x68, VK_BROWSER_STOP         | KBDEXT        },
        { 0x69, VK_BROWSER_FORWARD      | KBDEXT        },
        { 0x6A, VK_BROWSER_BACK         | KBDEXT        },
        { 0x6B, VK_LAUNCH_APP1          | KBDEXT        },
        { 0x6C, VK_LAUNCH_MAIL          | KBDEXT        },
        { 0x6D, VK_LAUNCH_MEDIA_SELECT  | KBDEXT        },
#endif /* _WIN32_WINNT >= 0x0500 */
        { 0,    0               }
};

// same as e0prefixToVirtualKeys, but with 0xE1 as prefix
// break is sent as e1+ctrl+num lock
static LDATA VSC_VK e1prefixToVirtualKeys[] = {
        { 0x1D, VK_PAUSE        },
        { 0,    0               }
};

// first field is a dead key combination: character goes in
// the higher word, dead key in the lower word. see the deadKeyNames
// table to find the codes for each dead key. second field is the
// character that this combination should produce. the third field
// is a flag which doesn't seem to be used in western keyboards,
// but I guess if you set it to DKF_DEAD then the combination turns
// into yet another dead key (the character is the dead key that is
// produced, and another lookup is eventually performed)
// note that the dead key itself is specified in combination with
// space, which is the only way to print it
static LDATA DEADKEY deadKeys[] = {
        // tilde: ~
        { MAKELONG( L'a', L'~' ), 0x00E3, 0 },
        { MAKELONG( L'A', L'~' ), 0x00C3, 0 },
        { MAKELONG( L'n', L'~' ), 0x00F1, 0 },
        { MAKELONG( L'N', L'~' ), 0x00D1, 0 },
        { MAKELONG( L'o', L'~' ), 0x00F5, 0 },
        { MAKELONG( L'O', L'~' ), 0x00D5, 0 },
        { MAKELONG( L' ', L'~' ), L'~',   0 },
        // diaeresis: ¨
        { MAKELONG( L'a', 0x00a8 ), 0x00E4, 0 },
        { MAKELONG( L'e', 0x00a8 ), 0x00EB, 0 },
        { MAKELONG( L'i', 0x00a8 ), 0x00EF, 0 },
        { MAKELONG( L'o', 0x00a8 ), 0x00F6, 0 },
        { MAKELONG( L'u', 0x00a8 ), 0x00FC, 0 },
        { MAKELONG( L'y', 0x00a8 ), 0x00FF, 0 },
        { MAKELONG( L'A', 0x00a8 ), 0x00C4, 0 },
        { MAKELONG( L'E', 0x00a8 ), 0x00CB, 0 },
        { MAKELONG( L'I', 0x00a8 ), 0x00CF, 0 },
        { MAKELONG( L'O', 0x00a8 ), 0x00D6, 0 },
        { MAKELONG( L'U', 0x00a8 ), 0x00DC, 0 },
        { MAKELONG( L' ', 0x00a8 ), 0x00A8, 0 },
        // acute accent: ´
        { MAKELONG( L'a', 0x00b4 ), 0x00E1, 0 },
        { MAKELONG( L'e', 0x00b4 ), 0x00E9, 0 },
        { MAKELONG( L'i', 0x00b4 ), 0x00ED, 0 },
        { MAKELONG( L'o', 0x00b4 ), 0x00F3, 0 },
        { MAKELONG( L'u', 0x00b4 ), 0x00FA, 0 },
        { MAKELONG( L'y', 0x00b4 ), 0x00FD, 0 },
        { MAKELONG( L'A', 0x00b4 ), 0x00C1, 0 },
        { MAKELONG( L'E', 0x00b4 ), 0x00C9, 0 },
        { MAKELONG( L'I', 0x00b4 ), 0x00CD, 0 },
        { MAKELONG( L'O', 0x00b4 ), 0x00D3, 0 },
        { MAKELONG( L'U', 0x00b4 ), 0x00DA, 0 },
        { MAKELONG( L'Y', 0x00b4 ), 0x00DD, 0 },
        { MAKELONG( L' ', 0x00b4 ), 0x00B4, 0 },
        // grave accent: `
        { MAKELONG( L'a', L'`' ), 0x00E0, 0 },
        { MAKELONG( L'e', L'`' ), 0x00E8, 0 },
        { MAKELONG( L'i', L'`' ), 0x00EC, 0 },
        { MAKELONG( L'o', L'`' ), 0x00F2, 0 },
        { MAKELONG( L'u', L'`' ), 0x00F9, 0 },
        { MAKELONG( L'A', L'`' ), 0x00C0, 0 },
        { MAKELONG( L'E', L'`' ), 0x00C8, 0 },
        { MAKELONG( L'I', L'`' ), 0x00CC, 0 },
        { MAKELONG( L'O', L'`' ), 0x00D2, 0 },
        { MAKELONG( L'U', L'`' ), 0x00D9, 0 },
        { MAKELONG( L' ', L'`' ), L'`',   0 },
        // cedilla: ¸
        { MAKELONG( L'c', 0x00B8 ), 0x00E7, 0 },
        { MAKELONG( L'C', 0x00B8 ), 0x00C7, 0 },
        { MAKELONG( L'g', 0x00B8 ), 0x0123, 0 },
        { MAKELONG( L'G', 0x00B8 ), 0x0122, 0 },
        { MAKELONG( L'k', 0x00B8 ), 0x0137, 0 },
        { MAKELONG( L'K', 0x00B8 ), 0x0136, 0 },
        { MAKELONG( L'l', 0x00B8 ), 0x013C, 0 },
        { MAKELONG( L'L', 0x00B8 ), 0x013B, 0 },
        { MAKELONG( L'n', 0x00B8 ), 0x0146, 0 },
        { MAKELONG( L'N', 0x00B8 ), 0x0145, 0 },
        { MAKELONG( L's', 0x00B8 ), 0x015F, 0 },
        { MAKELONG( L'S', 0x00B8 ), 0x015E, 0 },
        { MAKELONG( L't', 0x00B8 ), 0x0163, 0 },
        { MAKELONG( L'T', 0x00B8 ), 0x0162, 0 },
        { MAKELONG( L' ', 0x00B8 ), 0x00B8, 0 },
        // caron:
        { MAKELONG( L'c', 0x02C7 ), 0x010D, 0 },
        { MAKELONG( L'C', 0x02C7 ), 0x010C, 0 },
        { MAKELONG( L'd', 0x02C7 ), 0x010F, 0 },
        { MAKELONG( L'D', 0x02C7 ), 0x010E, 0 },
        { MAKELONG( L'e', 0x02C7 ), 0x011B, 0 },
        { MAKELONG( L'E', 0x02C7 ), 0x011A, 0 },
        { MAKELONG( L'n', 0x02C7 ), 0x0148, 0 },
        { MAKELONG( L'N', 0x02C7 ), 0x0147, 0 },
        { MAKELONG( L'r', 0x02C7 ), 0x0159, 0 },
        { MAKELONG( L'R', 0x02C7 ), 0x0158, 0 },
        { MAKELONG( L's', 0x02C7 ), 0x0161, 0 },
        { MAKELONG( L'S', 0x02C7 ), 0x0160, 0 },
        { MAKELONG( L't', 0x02C7 ), 0x0165, 0 },
        { MAKELONG( L'T', 0x02C7 ), 0x0164, 0 },
        { MAKELONG( L'u', 0x02C7 ), 0x01D4, 0 },
        { MAKELONG( L'U', 0x02C7 ), 0x01D3, 0 },
        { MAKELONG( L'z', 0x02C7 ), 0x017E, 0 },
        { MAKELONG( L'Z', 0x02C7 ), 0x017D, 0 },
        { MAKELONG( L' ', 0x02C7 ), 0x02C7, 0 },
        // circumflex accent: ^
        { MAKELONG( L'a', L'^' ), 0x00E2, 0 },
        { MAKELONG( L'e', L'^' ), 0x00EA, 0 },
        { MAKELONG( L'i', L'^' ), 0x00EE, 0 },
        { MAKELONG( L'o', L'^' ), 0x00F4, 0 },
        { MAKELONG( L'u', L'^' ), 0x00FB, 0 },
        { MAKELONG( L'c', L'^' ), 0x0109, 0 },
        { MAKELONG( L'g', L'^' ), 0x011D, 0 },
        { MAKELONG( L'h', L'^' ), 0x0125, 0 },
        { MAKELONG( L'j', L'^' ), 0x0135, 0 },
        { MAKELONG( L's', L'^' ), 0x015D, 0 },
        { MAKELONG( L'A', L'^' ), 0x00C2, 0 },
        { MAKELONG( L'E', L'^' ), 0x00CA, 0 },
        { MAKELONG( L'I', L'^' ), 0x00CE, 0 },
        { MAKELONG( L'O', L'^' ), 0x00D4, 0 },
        { MAKELONG( L'U', L'^' ), 0x00DB, 0 },
        { MAKELONG( L'C', L'^' ), 0x0108, 0 },
        { MAKELONG( L'G', L'^' ), 0x011C, 0 },
        { MAKELONG( L'H', L'^' ), 0x0124, 0 },
        { MAKELONG( L'J', L'^' ), 0x0134, 0 },
        { MAKELONG( L'S', L'^' ), 0x015C, 0 },
        { MAKELONG( L' ', L'^' ), L'^',   0 },
        // macron:
        { MAKELONG( L'a', 0x02C9 ), 0x0101, 0 },
        { MAKELONG( L'e', 0x02C9 ), 0x0113, 0 },
        { MAKELONG( L'i', 0x02C9 ), 0x012B, 0 },
        { MAKELONG( L'u', 0x02C9 ), 0x016B, 0 },
        { MAKELONG( L'A', 0x02C9 ), 0x0100, 0 },
        { MAKELONG( L'E', 0x02C9 ), 0x0112, 0 },
        { MAKELONG( L'I', 0x02C9 ), 0x012A, 0 },
        { MAKELONG( L'U', 0x02C9 ), 0x016A, 0 },
        { MAKELONG( L' ', 0x02C9 ), 0x02C9, 0 },
        // greek lowercase:
        { MAKELONG( L'a', greek ), 0x03B1, 0 }, // alpha
        { MAKELONG( L'b', greek ), 0x03B2, 0 }, // beta
        { MAKELONG( L'g', greek ), 0x03B3, 0 }, // gamma
        { MAKELONG( L'd', greek ), 0x03B4, 0 }, // delta
        { MAKELONG( L'e', greek ), 0x03B5, 0 }, // epsilon
        { MAKELONG( L'z', greek ), 0x03B6, 0 }, // zeta
        { MAKELONG( L'h', greek ), 0x03B7, 0 }, // eta
        { MAKELONG( L'f', greek ), 0x03B8, 0 }, // theta
        { MAKELONG( L'i', greek ), 0x03B9, 0 }, // iota
        { MAKELONG( L'k', greek ), 0x03BA, 0 }, // kappa
        { MAKELONG( L'l', greek ), 0x03BB, 0 }, // lambda
        { MAKELONG( L'm', greek ), 0x03BC, 0 }, // mu
        { MAKELONG( L'n', greek ), 0x03BD, 0 }, // nu
        { MAKELONG( L'q', greek ), 0x03BE, 0 }, // xi
        { MAKELONG( L'o', greek ), 0x03BF, 0 }, // omikron
        { MAKELONG( L'p', greek ), 0x03C0, 0 }, // pi
        { MAKELONG( L'r', greek ), 0x03C1, 0 }, // rho
        { MAKELONG( L's', greek ), 0x03C3, 0 }, // sigma
        { MAKELONG( L't', greek ), 0x03C4, 0 }, // tau
        { MAKELONG( L'u', greek ), 0x03C5, 0 }, // upsilon
        { MAKELONG( L'v', greek ), 0x03C6, 0 }, // phi
        { MAKELONG( L'x', greek ), 0x03C7, 0 }, // chi
        { MAKELONG( L'y', greek ), 0x03C8, 0 }, // psi
        { MAKELONG( L'w', greek ), 0x03C9, 0 }, // omega
        // greek uppercase:
        { MAKELONG( L'A', greek ), 0x0391, 0 }, // alpha
        { MAKELONG( L'B', greek ), 0x0392, 0 }, // beta
        { MAKELONG( L'G', greek ), 0x0393, 0 }, // gamma
        { MAKELONG( L'D', greek ), 0x0394, 0 }, // delta
        { MAKELONG( L'E', greek ), 0x0395, 0 }, // epsilon
        { MAKELONG( L'Z', greek ), 0x0396, 0 }, // zeta
        { MAKELONG( L'H', greek ), 0x0397, 0 }, // eta
        { MAKELONG( L'F', greek ), 0x0398, 0 }, // theta
        { MAKELONG( L'I', greek ), 0x0399, 0 }, // iota
        { MAKELONG( L'K', greek ), 0x039A, 0 }, // kappa
        { MAKELONG( L'L', greek ), 0x039B, 0 }, // lambda
        { MAKELONG( L'M', greek ), 0x039C, 0 }, // mu
        { MAKELONG( L'N', greek ), 0x039D, 0 }, // nu
        { MAKELONG( L'Q', greek ), 0x039E, 0 }, // xi
        { MAKELONG( L'O', greek ), 0x039F, 0 }, // omikron
        { MAKELONG( L'P', greek ), 0x03A0, 0 }, // pi
        { MAKELONG( L'R', greek ), 0x03A1, 0 }, // rho
        { MAKELONG( L'S', greek ), 0x03A3, 0 }, // sigma
        { MAKELONG( L'T', greek ), 0x03A4, 0 }, // tau
        { MAKELONG( L'U', greek ), 0x03A5, 0 }, // upsilon
        { MAKELONG( L'V', greek ), 0x03A6, 0 }, // phi
        { MAKELONG( L'X', greek ), 0x03A7, 0 }, // chi
        { MAKELONG( L'Y', greek ), 0x03A8, 0 }, // psi
        { MAKELONG( L'W', greek ), 0x03A9, 0 }, // omega
        //--------------------------------------
        // not all symbols are implemented by the standard fonts, and I haven't
        // bothered to look up the Unicode, because they cannot be displayed anyway
        { MAKELONG( L'a', math ), 0x0000, 0 },  // angle
        { MAKELONG( L'A', math ), 0x0000, 0 },  // for all
        { MAKELONG( L'c', math ), 0x0000, 0 },  // subset of
        { MAKELONG( L'C', math ), 0x0000, 0 },  // includes
        { MAKELONG( L'd', math ), 0x2202, 0 },  // partial differential
        { MAKELONG( L'D', math ), 0x2206, 0 },  // increment
        { MAKELONG( L'e', math ), 0x0000, 0 },  // element of
        { MAKELONG( L'E', math ), 0x0000, 0 },  // exists
        { MAKELONG( L'i', math ), 0x221E, 0 },  // infinity
        { MAKELONG( L'I', math ), 0x222B, 0 },  // integral
        { MAKELONG( L'n', math ), 0x00AC, 0 },  // logical not
        { MAKELONG( L'N', math ), 0x2310, 0 },  // reversed not
        { MAKELONG( L'o', math ), 0x0000, 0 },  // empty set
        { MAKELONG( L'p', math ), 0x0000, 0 },  // power-set
        { MAKELONG( L'P', math ), 0x220F, 0 },  // n-ary product
        { MAKELONG( L'r', math ), 0x221A, 0 },  // square root
        { MAKELONG( L'S', math ), 0x2211, 0 },  // n-ary summation
        { MAKELONG( L'u', math ), 0x0000, 0 },  // union
        { MAKELONG( L'U', math ), 0x2229, 0 },  // intersection
        { MAKELONG( L'v', math ), 0x0000, 0 },  // logical-or
        { MAKELONG( L'V', math ), 0x0000, 0 },  // logical-and
        { MAKELONG( L'x', math ), 0x0000, 0 },  // cross product
        { MAKELONG( L'.', math ), 0x00B7, 0 },  // dot product
        { MAKELONG( L'<', math ), 0x2264, 0 },  // less-than or equal to
        { MAKELONG( L'>', math ), 0x2265, 0 },  // greater-than or equal to
        { MAKELONG( L'/', math ), 0x00F7, 0 },  // division
        { MAKELONG( L'+', math ), 0x00B1, 0 },  // plus-minus sign
        { MAKELONG( L'=', math ), 0x2248, 0 },  // almost equal to
        { MAKELONG( L'9', math ), 0x2260, 0 },  // not equal to
        { MAKELONG( L'£', math ), 0x2261, 0 },  // identical to
        { MAKELONG( L'2', math ), 0x0000, 0 },  // double arrow down
        { MAKELONG( L'4', math ), 0x0000, 0 },  // double arrow left
        { MAKELONG( L'6', math ), 0x0000, 0 },  // double arrow right
        { MAKELONG( L'8', math ), 0x0000, 0 },  // double arrow up
        { MAKELONG( L'%', math ), 0x2030, 0 },  // per-mille sign
        //--------------------------------------
        // typing a key with a modifier takes at least two and sometimes three times
        // as long as a non-modified one. using a compose/multi key is thus not
        // necessarily slower than using dead keys with AltGr. if you live in a place
        // where accented characters are used, odds are good that you also have a
        // keyboard with 102 characters, which makes this key a good candidate.
        // see <http://webcvs.freedesktop.org/xorg/xc/nls/Compose/en_US.UTF-8?view=co>

        // compose: accents
        // reuse the dead keys tables as far as possible, i.e. compose+accent key
        // is functionally equivalent to pressing the accent dead key.
        { MAKELONG( L'~',   compose ), L'~',   DKF_DEAD },      // tilde
        { MAKELONG( L'`',   compose ), L'`',   DKF_DEAD },      // grave accent
        { MAKELONG( L'\'',  compose ), 0x00B4, DKF_DEAD },      // acute accent
        { MAKELONG( L'\"',  compose ), 0x00A8, DKF_DEAD },      // diaeresis
        { MAKELONG( L':',   compose ), 0x00A8, DKF_DEAD },      // diaeresis, alternative
        { MAKELONG( L';',   compose ), 0x00A8, DKF_DEAD },      // diaeresis, alternative
        { MAKELONG( L',',   compose ), 0x00B8, DKF_DEAD },      // cedilla
        { MAKELONG( L'^',   compose ), L'^',   DKF_DEAD },      // circumflex
        { MAKELONG( L'c',   compose ), 0x02C7, DKF_DEAD },      // caron
        { MAKELONG( L'-',   compose ), 0x02C9, DKF_DEAD },      // macron

        // compose: ligatures
        // character representing the first part spawn a dead key of that ASCII
        // value although there is no way to enter that dead key directly on the
        // keyboard as is the case with the accents. thus, the dead keys referred
        // to below can only be accessed as part of a compose combination.
        // both capital and small letter maps to the dead key of the small letter
        // so that case (of the first letter) doesn't matter.
        { MAKELONG( L'*',   compose ), L'*',   DKF_DEAD },      // ring above
        { MAKELONG( L'0',   compose ), L'*',   DKF_DEAD },      // ring above, alternative
        { MAKELONG( L'/',   compose ), L'/',   DKF_DEAD },      // stroke
        { MAKELONG( L'a',   compose ), L'a',   DKF_DEAD },
        { MAKELONG( L'A',   compose ), L'a',   DKF_DEAD },
        { MAKELONG( L'o',   compose ), L'o',   DKF_DEAD },
        { MAKELONG( L'O',   compose ), L'o',   DKF_DEAD },
        { MAKELONG( L'@',   compose ), L'a',   DKF_DEAD },      // a, alternative
        { MAKELONG( L't',   compose ), L't',   DKF_DEAD },
        { MAKELONG( L'T',   compose ), L't',   DKF_DEAD },
        { MAKELONG( L's',   compose ), L's',   DKF_DEAD },

        // compose: altgr variants of the characters on the home row (second column
        // below is joined to the third column above; first column above and then first
        // column below, gives the character code of the third column below)
        { MAKELONG( L'a',   L'*'    ), 0x00E5, 0        },      // small a with ring above
        { MAKELONG( L'A',   L'*'    ), 0x00C5, 0        },      // capital a with ring above
        { MAKELONG( L'a',   L'a'    ), 0x00E5, 0        },      // small a with ring above, alt. 1
        { MAKELONG( L'A',   L'a'    ), 0x00C5, 0        },      // capital a with ring above, alt. 1
        { MAKELONG( L'o',   L'/'    ), 0x00F8, 0        },      // small o with stroke
        { MAKELONG( L'O',   L'/'    ), 0x00D8, 0        },      // capital o with stroke
        { MAKELONG( L'a',   L'o'    ), 0x00E5, 0        },      // small a with ring above, alt. 2
        { MAKELONG( L'A',   L'o'    ), 0x00C5, 0        },      // capital a with ring above, alt. 2
        { MAKELONG( L'e',   L'o'    ), 0x00F8, 0        },      // small o with stroke, alt.
        { MAKELONG( L'E',   L'o'    ), 0x00D8, 0        },      // capital o with stroke, alt.
        { MAKELONG( L'e',   L'a'    ), 0x00E6, 0        },      // small ae ligature
        { MAKELONG( L'E',   L'a'    ), 0x00C6, 0        },      // capital ae ligature
        { MAKELONG( L'd',   0x02C9  ), 0x00F0, 0        },      // small eth   (-d)
        { MAKELONG( L'D',   0x02C9  ), 0x00D0, 0        },      // capital eth (-D)
        { MAKELONG( L'h',   L't'    ), 0x00FE, 0        },      // small thorn
        { MAKELONG( L'H',   L't'    ), 0x00DE, 0        },      // capital thorn
        { MAKELONG( L's',   L's'    ), 0x00DF, 0        },      // sharp s

        // compose: Polish-Lithuanian
        { MAKELONG( L'l',   L'/'    ), 0x0142, 0        },      // l with stroke
        { MAKELONG( L'L',   L'/'    ), 0x0141, 0        },
        { MAKELONG( L'.',   compose ), 0x02D9, DKF_DEAD },      // dot above
        { MAKELONG( L'e',   0x02D9  ), 0x0117, 0        },      // e with dot above
        { MAKELONG( L'E',   0x02D9  ), 0x0116, 0        },
        { MAKELONG( L'z',   0x02D9  ), 0x017C, 0        },      // z with dot above
        { MAKELONG( L'Z',   0x02D9  ), 0x017B, 0        },
        { MAKELONG( L' ',   0x02D9  ), 0x02D9, 0        },
        { MAKELONG( L'a',   0x00B8  ), 0x0105, 0        },      // a with ogonek
        { MAKELONG( L'A',   0x00B8  ), 0x0104, 0        },
        { MAKELONG( L'e',   0x00B8  ), 0x0119, 0        },      // e with ogonek
        { MAKELONG( L'E',   0x00B8  ), 0x0118, 0        },
        { MAKELONG( L'i',   0x00B8  ), 0x012F, 0        },      // i with nosine
        { MAKELONG( L'I',   0x00B8  ), 0x012E, 0        },
        { MAKELONG( L'u',   0x00B8  ), 0x0173, 0        },      // u with nosine
        { MAKELONG( L'U',   0x00B8  ), 0x0172, 0        },
        { MAKELONG( L'c',   0x00B4  ), 0x0107, 0        },      // c with acute
        { MAKELONG( L'C',   0x00B4  ), 0x0106, 0        },
        { MAKELONG( L'n',   0x00B4  ), 0x0144, 0        },      // n with acute
        { MAKELONG( L'N',   0x00B4  ), 0x0143, 0        },
        { MAKELONG( L's',   0x00B4  ), 0x015B, 0        },      // s with acute
        { MAKELONG( L'S',   0x00B4  ), 0x015A, 0        },
        { MAKELONG( L'z',   0x00B4  ), 0x017A, 0        },      // z with acute
        { MAKELONG( L'Z',   0x00B4  ), 0x0179, 0        },

        // compose: Czech
        { MAKELONG( L'u',   L'*'    ), 0x016F, 0        },      // u with ring above
        { MAKELONG( L'U',   L'*'    ), 0x016E, 0        },

        // compose: Hungarian
        { MAKELONG( L'=',   compose ), 0x02DD, DKF_DEAD },      // double acute
        { MAKELONG( L'o',   0x02DD  ), 0x0151, 0        },      // o with double acute
        { MAKELONG( L'O',   0x02DD  ), 0x0150, 0        },

        // compose: Yugoslav
        { MAKELONG( L'd',   L'/'    ), 0x0111, 0        },      // d with stroke
        { MAKELONG( L'D',   L'/'    ), 0x0110, 0        },

        // compose: Romanian
        { MAKELONG( L'(',   compose ), 0x02D8, DKF_DEAD },      // breve
        { MAKELONG( L'a',   0x02D8  ), 0x0103, 0        },      // a with breve
        { MAKELONG( L'A',   0x02D8  ), 0x0102, 0        },
        // hack: <comma> <s> is already taken; however, s with dot above
        // is not in regular use, and is close enough to pass
        { MAKELONG( L's',   0x02D9  ), 0x0219, 0        },      // s with comma below
        { MAKELONG( L'S',   0x02D9  ), 0x0218, 0        },
        { MAKELONG( L't',   0x02D9  ), 0x021B, 0        },      // t with comma below
        { MAKELONG( L'T',   0x02D9  ), 0x021A, 0        },

        // compose: Esperanto
        { MAKELONG( L'u',   0x02D8  ), 0x016D, 0        },      // u with breve
        { MAKELONG( L'U',   0x02D8  ), 0x016C, 0        },

        // compose: Turkish Latin
        { MAKELONG( L'g',   0x02D8  ), 0x011F, 0        },      // g with breve
        { MAKELONG( L'G',   0x02D8  ), 0x011E, 0        },
        { MAKELONG( L'i',   0x02D9  ), 0x0131, 0        },      // dotless i
        { MAKELONG( L'I',   0x02D9  ), 0x0130, 0        },      // i with dot above

        // compose: Maltese
        { MAKELONG( L'h',   L'/'    ), 0x0127, 0        },      // h with stroke
        { MAKELONG( L'H',   L'/'    ), 0x0126, 0        },
        { MAKELONG( L'c',   0x02D9  ), 0x010B, 0        },      // c with dot above
        { MAKELONG( L'C',   0x02D9  ), 0x010A, 0        },
        { MAKELONG( L'g',   0x02D9  ), 0x0121, 0        },      // g with dot above
        { MAKELONG( L'G',   0x02D9  ), 0x0120, 0        },
        { MAKELONG( L'z',   0x02D9  ), 0x017C, 0        },      // z with dot above
        { MAKELONG( L'Z',   0x02D9  ), 0x017B, 0        },

        // compose: miscellaneous
        { MAKELONG( L'!',   compose ), L'!',   DKF_DEAD },
        { MAKELONG( L'!',   L'!'    ), 0x00A1, 0        },      // inverted exclamation mark
        { MAKELONG( L'|',   compose ), L'|',   DKF_DEAD },
        { MAKELONG( L'c',   L'|'    ), 0x00A2, 0        },      // cent
        { MAKELONG( L'L',   0x02C9  ), 0x00A3, 0        },      // pound   (-L)
        { MAKELONG( L'Y',   0x02C9  ), 0x00A5, 0        },      // yen     (-Y)
        { MAKELONG( L'-',   0x02C9  ), L'-',   DKF_DEAD },      // dash    (--)
        { MAKELONG( L'-',   L'-'    ), 0x2014, 0        },      // em dash (---), alt. 1
        { MAKELONG( L'm',   0x02C9  ), 0x2014, 0        },      // em dash (-m),  alt. 2
        { MAKELONG( L'.',   L'-'    ), 0x2013, 0        },      // en dash (--.), alt. 1
        { MAKELONG( L'n',   0x02C9  ), 0x2013, 0        },      // en dash (-n),  alt. 2
        { MAKELONG( L'o',   L's'    ), 0x00A7, 0        },      // paragraph
        { MAKELONG( L'O',   compose ), L'o',   DKF_DEAD },
        { MAKELONG( L'c',   L'o'    ), 0x00A9, 0        },      // copyright
        { MAKELONG( L'R',   L'o'    ), 0x00AE, 0        },      // registered
        { MAKELONG( L'p',   compose ), L'p',   DKF_DEAD },
        { MAKELONG( L'!',   L'p'    ), 0x00B6, 0        },      // pilcrow
        { MAKELONG( L'?',   compose ), L'?',   DKF_DEAD },
        { MAKELONG( L'?',   L'?'    ), 0x00BF, 0        },      // inverted question mark
        { MAKELONG( L'C',   compose ), L'c',   DKF_DEAD },
        { MAKELONG( L'=',   L'c'    ), 0x20AC, 0        },      // euro
        { MAKELONG( L'M',   L't'    ), 0x2122, 0        },      // trademark
        { MAKELONG( L'<',   compose ), L'<',   DKF_DEAD },
        { MAKELONG( L'<',   L'<'    ), 0x00AB, 0        },      // left guillemet
        { MAKELONG( L'"',   L'<'    ), 0x201C, 0        },      // left double quotation mark
        { MAKELONG( L'\'',  L'<'    ), 0x2018, 0        },      // left single quotation mark
        { MAKELONG( L'>',   compose ), L'>',   DKF_DEAD },
        { MAKELONG( L'>',   L'>'    ), 0x00BB, 0        },      // right guillemet
        { MAKELONG( L'"',   L'>'    ), 0x201D, 0        },      // right double quotation mark
        { MAKELONG( L'\'',  L'>'    ), 0x2019, 0        },      // right single quotation mark

        // compose: cancellation
        // compose cancels itself, escape cancels any dead key. compose could be
        // made to cancel diacritics too, but it would then apply to any dead key
        // since compose plus a diacritic maps them, possibly being confusing if
        // the user types a dead key and then wants to start a compose sequence.
        { MAKELONG( compose, compose ), cancel, 0 },
        { MAKELONG( escape,  compose ), cancel, 0 },
        { MAKELONG( escape,  L'*'    ), cancel, 0 },
        { MAKELONG( escape,  L'/'    ), cancel, 0 },
        { MAKELONG( escape,  0x02C9  ), cancel, 0 },            // -
        { MAKELONG( escape,  L'-'    ), cancel, 0 },            // -- (note: twice!)
        { MAKELONG( escape,  0x02DD  ), cancel, 0 },            // =
        { MAKELONG( escape,  L'|'    ), cancel, 0 },
        { MAKELONG( escape,  L'?'    ), cancel, 0 },
        { MAKELONG( escape,  L'!'    ), cancel, 0 },
        { MAKELONG( escape,  L'@'    ), cancel, 0 },
        { MAKELONG( escape,  L'~'    ), cancel, 0 },
        { MAKELONG( escape,  L'`'    ), cancel, 0 },
        { MAKELONG( escape,  0x00B4  ), cancel, 0 },            // '
        { MAKELONG( escape,  0x00A8  ), cancel, 0 },            // "
        { MAKELONG( escape,  0x00B8  ), cancel, 0 },            // ,
        { MAKELONG( escape,  0x02D9  ), cancel, 0 },            // .
        { MAKELONG( escape,  L'^'    ), cancel, 0 },
        { MAKELONG( escape,  L'a'    ), cancel, 0 },
        { MAKELONG( escape,  L'o'    ), cancel, 0 },
        { MAKELONG( escape,  L'e'    ), cancel, 0 },
        { MAKELONG( escape,  L'p'    ), cancel, 0 },
        { MAKELONG( escape,  L't'    ), cancel, 0 },
        { MAKELONG( escape,  L's'    ), cancel, 0 },
        { MAKELONG( escape,  0x02C7  ), cancel, 0 },            // c
        { MAKELONG( escape,  0x02D8  ), cancel, 0 },            // (
        { MAKELONG( escape,  L'<'    ), cancel, 0 },
        { MAKELONG( escape,  L'>'    ), cancel, 0 },

        // TODO: compose+letter should give a dead key that corresponds
        // to the ctrl+alt version of that letter, e.g. compose+a should
        // give dead aring, and dead aring+e should then give aelig.
        
        // end of table
        0, 0
};

// keys that are non-printable have their name listed here
// instead. the first column is the scan code for the
// key, and the second is the description. note that this is
// not an array of a structure, but rather a plain array.
// this is essentially scanCodesToVirtualKeys with a description.
static LDATA VSC_LPWSTR keyNames[] = {
        0x01    /* VK_ESCAPE            */,     L"Escape",
        0x02    /* VK_OEM_4             */,     L"Ampersand",
        0x03    /* VK_'7'               */,     L"Left Bracket",
        0x04    /* VK_'5'               */,     L"Left Brace",
        0x05    /* VK_'3'               */,     L"Right Brace",
        0x06    /* VK_'1'               */,     L"Left Parenthesis",
        0x07    /* VK_'9'               */,     L"Equal Sign",
        0x08    /* VK_'0'               */,     L"Asterisk",
        0x09    /* VK_'2'               */,     L"Right Parenthesis",
        0x0A    /* VK_'4'               */,     L"Plus",
        0x0B    /* VK_'6'               */,     L"Right Bracket",
        0x0C    /* VK_'8'               */,     L"Exclamation Point",
        0x0D    /* VK_OEM_8             */,     L"Hash",
        0x0E    /* VK_BACK              */,     L"Backspace",
        0x0F    /* VK_TAB               */,     L"Tab",
        0x10    /* VK_OEM_1             */,     L"Semicolon",
        0x11    /* VK_OEM_COMMA         */,     L"Comma",
        0x12    /* VK_OEM_PERIOD        */,     L"Period",
        0x1A    /* VK_OEM_2             */,     L"Slash",
        0x1B    /* VK_OEM_6             */,     L"At",
        0x1C    /* VK_RETURN            */,     L"Return",
        0x1D    /* VK_LCONTROL          */,     L"Left Control",
        0x28    /* VK_OEM_MINUS         */,     L"Dash",
        0x29    /* VK_OEM_3             */,     L"Dollar",
        0x2A    /* VK_LSHIFT            */,     L"Left Shift",
        0x2B    /* VK_OEM_5             */,     L"Backslash",
        0x2C    /* VK_OEM_7             */,     L"Single Quote",        
        0x36    /* VK_RSHIFT            */,     L"Right Shift",
        0x37    /* VK_MULTIPLY          */,     L"Numeric *",
        0x38    /* VK_LMENU             */,     L"Alt",
        0x39    /* VK_SPACE             */,     L"Space",
        0x3A    /* VK_CAPITAL           */,     L"Caps Lock",
        0x3B    /* VK_F1                */,     L"Function Key 1",
        0x3C    /* VK_F2                */,     L"Function Key 2",
        0x3D    /* VK_F3                */,     L"Function Key 3",
        0x3E    /* VK_F4                */,     L"Function Key 4",
        0x3F    /* VK_F5                */,     L"Function Key 5",
        0x40    /* VK_F6                */,     L"Function Key 6",
        0x41    /* VK_F7                */,     L"Function Key 7",
        0x42    /* VK_F8                */,     L"Function Key 8",
        0x43    /* VK_F9                */,     L"Function Key 9",
        0x44    /* VK_F10               */,     L"Function Key 10",
        // Pause is E1+Ctrl+NumLock
        0x45    /* VK_NUMLOCK           */,     L"Pause",
        0x46    /* VK_SCROLL            */,     L"Scroll Lock",
        // just as the code mapping is not specified with the VK_NUMPAD? symbols,
        // neither are the names (see the scanCodesToVirtualKeys table for details).
#ifdef NO_ATM
        0x47    /* VK_HOME              */,     L"Numeric 7",
        0x48    /* VK_UP                */,     L"Numeric 8",
        0x49    /* VK_PRIOR             */,     L"Numeric 9",
        0x4A    /* VK_SUBTRACT          */,     L"Numeric -",
        0x4B    /* VK_LEFT              */,     L"Numeric 4",
        0x4C    /* VK_CLEAR             */,     L"Numeric 5",
        0x4D    /* VK_RIGHT             */,     L"Numeric 6",
        0x4E    /* VK_ADD               */,     L"Numeric +",
        0x4F    /* VK_END               */,     L"Numeric 1",
        0x50    /* VK_DOWN              */,     L"Numeric 2",
        0x51    /* VK_NEXT              */,     L"Numeric 3",
        0x52    /* VK_INSERT            */,     L"Numeric 0",
        0x53    /* VK_DELETE            */,     L"Numeric Delete",
#else
        0x47    /* VK_NUMPAD1           */,     L"Numeric 1",
        0x48    /* VK_NUMPAD2           */,     L"Numeric 2",
        0x49    /* VK_NUMPAD3           */,     L"Numeric 3",
        0x4A    /* VK_SUBTRACT          */,     L"Numeric -",
        0x4B    /* VK_NUMPAD4           */,     L"Numeric 4",
        0x4C    /* VK_NUMPAD5           */,     L"Numeric 5",
        0x4D    /* VK_NUMPAD6           */,     L"Numeric 6",
        0x4E    /* VK_ADD               */,     L"Numeric +",
        0x4F    /* VK_NUMPAD7           */,     L"Numeric 7",
        0x50    /* VK_NUMPAD8           */,     L"Numeric 8",
        0x51    /* VK_NUMPAD9           */,     L"Numeric 9",
        0x52    /* VK_NUMPAD0           */,     L"Numeric 0",
        0x53    /* VK_DECIMAL           */,     L"Decimal Separator",
#endif
        0x54    /* VK_SNAPSHOT          */,     L"System Request",
        0x56    /* VK_OEM_102           */,     L"Compose",
        0x57    /* VK_F11               */,     L"Function Key 11",
        0x58    /* VK_F12               */,     L"Function Key 12",
        0,                                      NULL
};

// this is the names of the keys with 0xE0 as scancode
// prefix (found in e0prefixToVirtualKeys[])
static LDATA VSC_LPWSTR extendedKeyNames[] = {
#if(_WIN32_WINNT >= 0x0500)
        0x10    /* VK_MEDIA_PREV_TRACK  */,     L"Previous Track",
        0x19    /* VK_MEDIA_NEXT_TRACK  */,     L"Next Track",
#endif /* _WIN32_WINNT >= 0x0500 */
        0x1C    /* VK_RETURN            */,     L"Enter",
        0x1D    /* VK_RCONTROL          */,     L"Right Control",
#if(_WIN32_WINNT >= 0x0500)
        0x20    /* VK_VOLUME_MUTE       */,     L"Mute",
        0x21    /* VK_LAUNCH_APP2       */,     L"Custom 2",
        0x22    /* VK_MEDIA_PLAY_PAUSE  */,     L"Pause",
        0x24    /* VK_MEDIA_STOP        */,     L"Stop",
        0x2E    /* VK_VOLUME_DOWN       */,     L"Volume Down",
        0x30    /* VK_VOLUME_UP         */,     L"Volume Up",
        0x32    /* VK_BROWSER_HOME      */,     L"Home",
#endif /* _WIN32_WINNT >= 0x0500 */
        0x35    /* VK_DIVIDE            */,     L"Numeric /",
        0x37    /* VK_SNAPSHOT          */,     L"Print Screen",
        0x38    /* VK_RMENU             */,     L"Alt Graph",
        0x45    /* VK_NUMLOCK           */,     L"Num Lock",
        0x46    /* VK_CANCEL            */,     L"Break",
        0x47    /* VK_HOME              */,     L"Home",
        0x48    /* VK_UP                */,     L"Up Arrow",
        0x49    /* VK_PRIOR             */,     L"Page Up",
        0x4B    /* VK_LEFT              */,     L"Left Arrow",
        0x4D    /* VK_RIGHT             */,     L"Right Arrow",
        0x4F    /* VK_END               */,     L"End",
        0x50    /* VK_DOWN              */,     L"Down Arrow",
        0x51    /* VK_NEXT              */,     L"Page Down",
        0x52    /* VK_INSERT            */,     L"Insert",
        0x53    /* VK_DELETE            */,     L"Delete",
        0x5B    /* VK_LWIN              */,     L"Left Windows",
        0x5C    /* VK_RWIN              */,     L"Right Windows",
        0x5D    /* VK_APPS              */,     L"Application",
#if(_WIN32_WINNT >= 0x0500)
        0x5F    /* VK_SLEEP             */,     L"Sleep",
        0x65    /* VK_BROWSER_SEARCH    */,     L"Search",
        0x66    /* VK_BROWSER_FAVORITES */,     L"Favorites",
        0x67    /* VK_BROWSER_REFRESH   */,     L"Reload",
        0x68    /* VK_BROWSER_STOP      */,     L"Stop",
        0x69    /* VK_BROWSER_FORWARD   */,     L"Forward",
        0x6A    /* VK_BROWSER_BACK      */,     L"Back",
        0x6B    /* VK_LAUNCH_APP1       */,     L"Custom 1",
        0x6C    /* VK_LAUNCH_MAIL       */,     L"Mail",
        0x6D    /* VK_LAUNCH_MEDIA_SELECT */,   L"Select",
#endif /* _WIN32_WINNT >= 0x0500 */
        0,                      NULL
};

// names of the dead keys themselves (not of the conjuncture)
// note that these are simple strings, where the first character
// in the string is the dead key. they are specified using string
// literals instead of numbers because the compiler will then
// concatenate them with the name
static LDATA LPWSTR deadKeyNames[] = {
        L"\x007E"       L"Tilde",                       // ~
        L"\x00A8"       L"Diaeresis",                   // ¨
        L"\x00B4"       L"Acute Accent",                // ´
        L"\x0060"       L"Grave Accent",                // `
        L"\x00B8"       L"Cedilla",                     // ¸
        L"\x02C7"       L"Caron",
        L"\x005E"       L"Circumflex Accent",           // ^
        L"\x0001"       L"Greek",
        L"\x0002"       L"Math",
        L"\x002F"       L"Stroke",
        L"\x002A"       L"Ring Above",
        L"\x02D9"       L"Dot Above",
        L"\x02DD"       L"Double Acute Accent",
        L"\x02C9"       L"Macron",
        L"\x02D8"       L"Breve",
        NULL
};

// enable AltGr to Ctrl + Alt translation, or not
#ifdef NO_RALT
#define USE_ALTGR KLLF_ALTGR
#else
#define USE_ALTGR       0
#endif

static LDATA KBDTABLES keyboardTables = {
        &modifiers,
        charTranslations,
        deadKeys,
        keyNames,
        extendedKeyNames,
        deadKeyNames,
        scanCodesToVirtualKeys,
        sizeof( scanCodesToVirtualKeys ) / sizeof( scanCodesToVirtualKeys[ 0 ] ),
        e0prefixToVirtualKeys,
        e1prefixToVirtualKeys,
#if(_WIN32_WINNT >= 0x0500)
        MAKELONG( USE_ALTGR, KBD_VERSION ),
        // ligatures are strokes that generate multiple characters (dead keys are
        // multiple strokes generating one character). the table seems to be built
        // this way: the first entry is the stroke as in the virtual key code, the
        // next is the modification number probably the index into the modifierBits[]
        // array, while the last the the characters to generate.
        // this could be used to e.g. insert entities (like &amp;,&lt;) when pressing a
        // stroke that is not allowed in the markup language (like &,<)
        0,
        0,
        NULL
#else
        USE_ALTGR
#endif /* _WIN32_WINNT >= 0x0500 */
};

// To compile for AMD64, your kbd.h should have the macro KBD_LONG_POINTER
// in front of all pointers and defined it to __ptr64 if BUILD_WOW6432 is
// defined. this will generate the right assembly code to return pointers.
// <http://www.levicki.net/articles/tips/2006/09/29/HOWTO_Build_keyboard_layouts_for_Windows_x64.php>

// a function called KbdLayerDescriptor must be exported
// as the first symbol in the library. this function must
// return the table containing the keyboard definition
PKBDTABLES KbdLayerDescriptor( VOID ) {
        return &keyboardTables;
}
