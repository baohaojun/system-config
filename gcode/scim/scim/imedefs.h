
/*************************************************
 *  imedefs.h                                    *
 *                                               *
 *  Copyright (C) 1995-1999 Microsoft Inc.       *
 *                                               *
 *************************************************/

#ifndef __IMEDEFS_H__
#define __IMEDEFS_H__
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 
#define NATIVE_CHARSET          GB2312_CHARSET
#define NATIVE_ANSI_CP          936
#define NATIVE_LANGUAGE         0x0804
// resource ID
#define IDI_IME                 0x0100


#define IDC_STATIC              -1

#define IDM_HLP                 0x0400
#define IDM_OPTGUD              0x0403
#define IDM_VER                 0x0401
#define IDM_PROP                0x0402

#define IDM_IME                 0x0450

#define DlgPROP                 101
#define DlgUIMODE               102
#define IDC_UIMODE1             1001
#define IDC_UIMODE2             1002
#define IDC_USER1               1003

// setting offset in .SET file
#define OFFSET_MODE_CONFIG      0
#define OFFSET_READLAYOUT       4

// state of composition
#define CST_INIT                0
#define CST_INPUT               1
#define CST_CHOOSE              2
#define CST_SYMBOL              3
#define CST_ALPHANUMERIC        7	// not in iImeState
#define CST_INVALID             8	// not in iImeState
#define CST_INVALID_INPUT       9	// not in iImeState
#define CST_CAPITAL             11

// state engin
#define ENGINE_COMP             0
#define ENGINE_ASCII            1
#define ENGINE_RESAULT          2
#define ENGINE_CHCAND           3
#define ENGINE_BKSPC            4
#define ENGINE_MULTISEL         5
#define ENGINE_ESC              6

#define VK_OEM_SEMICLN                  0xba	//  ;    :
#define VK_OEM_EQUAL                    0xbb	//  =    +
#define VK_OEM_SLASH                    0xbf	//  /    ?
#define VK_OEM_LBRACKET                 0xdb	//  [    {
#define VK_OEM_BSLASH                   0xdc	//  \    |
#define VK_OEM_RBRACKET                 0xdd	//  ]    }
#define VK_OEM_QUOTE                    0xde	//  '    "

#define CLASS_LEN               24


#define NEAR_CARET_FIRST_TIME   0x0001
#define NEAR_CARET_CANDIDATE    0x0002

typedef UINT u32;
typedef unsigned char u8;
typedef unsigned short u16;

typedef struct _tagImeG {
	int xChiCharWi;
	int yChiCharHi;

	int iPara;
	int iPerp;
	int iParaTol;
	int iPerpTol;
} IMEG;


typedef IMEG *PIMEG;
typedef IMEG NEAR *NPIMEG;
typedef IMEG FAR *LPIMEG;


typedef struct tagNEARCARET {	// for near caret offset calculatation
	int iLogFontFacX;
	int iLogFontFacY;
	int iParaFacX;
	int iPerpFacX;
	int iParaFacY;
	int iPerpFacY;
} NEARCARET;

typedef NEARCARET *PNEARCARET;
typedef NEARCARET NEAR *NPNEARCARET;
typedef NEARCARET FAR *LPNEARCARET;

extern HINSTANCE hInst;
extern IMEG sImeG;


extern TCHAR szUIClassName[];
extern TCHAR szCompClassName[];
extern TCHAR szStatusClassName[];
extern TCHAR *szImeName;




extern const NEARCARET ncUIEsc[], ncAltUIEsc[];
extern const POINT ptInputEsc[], ptAltInputEsc[];


LRESULT CALLBACK UIWndProc(HWND, u32, WPARAM, LPARAM);	// ui.c
LRESULT PASCAL UIPaint(HWND);	// ui.c


class input_context;

void PASCAL SetCompPosition(input_context&);	// compui.c
void PASCAL MoveDefaultCompPosition(HWND);	// compui.c
void PASCAL StartComp(HWND);	// compui.c
LRESULT CALLBACK CompWndProc(HWND, u32, WPARAM, LPARAM);	// compui.c

void show_status_wnd();
void hide_status_wnd();

void show_comp_wnd();
void hide_comp_wnd();

void PASCAL OpenStatus(HWND);	// statusui.c
LRESULT CALLBACK StatusWndProc(HWND, u32, WPARAM, LPARAM);

// dialog procedure
const char* msg_name(u32 msg);
extern HWND g_hCompWnd, g_hStatusWnd;
#endif
