#ifndef __IMEDEFS_H__
#define __IMEDEFS_H__
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 
#define NATIVE_CHARSET          GB2312_CHARSET
#define NATIVE_ANSI_CP          936
#define NATIVE_LANGUAGE         0x0804


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

extern HINSTANCE g_hInst;
extern IMEG sImeG;


extern const NEARCARET ncUIEsc[], ncAltUIEsc[];
extern const POINT ptInputEsc[], ptAltInputEsc[];


LRESULT CALLBACK UIWndProc(HWND, u32, WPARAM, LPARAM);	// ui.c


class input_context;

void PASCAL SetCompPosition(input_context&);	// compui.c
void PASCAL MoveDefaultCompPosition(HWND);	// compui.c
void PASCAL StartComp(HWND);	// compui.c
LRESULT CALLBACK CompWndProc(HWND, u32, WPARAM, LPARAM);	// compui.c

void show_status_wnd(HWND);
void hide_status_wnd(HWND);

void show_comp_wnd(HWND hUIWnd);
void hide_comp_wnd(HWND);

void PASCAL OpenStatus(HWND);	// statusui.c
LRESULT CALLBACK StatusWndProc(HWND, u32, WPARAM, LPARAM);

// dialog procedure
const char* msg_name(u32 msg);
#endif
