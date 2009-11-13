
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

#define IDS_STATUSERR           0x0200
#define IDS_CHICHAR             0x0201

#define IDS_EUDC                0x0202

#define IDS_USRDIC_FILTER       0x0210

#define IDS_FILE_OPEN_ERR       0x0220
#define IDS_MEM_LESS_ERR        0x0221

#define IDS_SETFILE             0x0300
#define IDS_IMENAME             0x0320
#define IDS_IMEUICLASS          0x0321
#define IDS_IMECOMPCLASS        0x0322
#define IDS_IMECANDCLASS        0x0323
#define IDS_IMESTATUSCLASS      0x0324
#define IDS_IMECMENUCLASS       0x0325
#define IDS_IMEREGNAME          0x0327
#define IDS_IMENAME_UNI         0x0330

#define IDS_WARN_OPENREG        0x0602


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

#define CANDPERPAGE             9

#define MAXSTRLEN               40
#define MAXCAND                 256
#define IME_MAXCAND             94
#define IME_XGB_MAXCAND         190
#define IME_UNICODE_MAXCAND     256
#define MAXCODE                 12

// set ime character
#define SIC_INIT                0
#define SIC_SAVE2               1
#define SIC_READY               2
#define SIC_MODIFY              3
#define SIC_SAVE1               4

#define BOX_UI                  0


#define IMEINDEXNUM             1

#define IME_CMODE_INDEX_FIRST   0x1000

#define INDEX_UNICODE           0


// border for UI
#define UI_MARGIN               4
#define COMP_TEXT_Y             17

#define STATUS_DIM_X            20
#define STATUS_DIM_Y            20
#define STATUS_NAME_MARGIN      20

#define CAND_START              1
#define uCandHome               0
#define uCandUp                 1
#define uCandDown               2
#define uCandEnd                3
#define CandGBinfoLen           64
// the flag for an opened or start UI
#define IMN_PRIVATE_UPDATE_STATUS             0x0001
#define IMN_PRIVATE_COMPOSITION_SIZE          0x0002
#define IMN_PRIVATE_UPDATE_QUICK_KEY          0x0004

#define MSG_ALREADY_OPEN                0x000001
#define MSG_ALREADY_OPEN2               0x000002
#define MSG_OPEN_CANDIDATE              0x000010
#define MSG_OPEN_CANDIDATE2             0x000020
#define MSG_CLOSE_CANDIDATE             0x000100
#define MSG_CLOSE_CANDIDATE2            0x000200
#define MSG_CHANGE_CANDIDATE            0x001000
#define MSG_CHANGE_CANDIDATE2           0x002000
#define MSG_ALREADY_START               0x010000
#define MSG_START_COMPOSITION           0x020000
#define MSG_END_COMPOSITION             0x040000
#define MSG_COMPOSITION                 0x080000
#define MSG_IMN_COMPOSITIONPOS          0x100000
#define MSG_IMN_UPDATE_STATUS           0x000400
#define MSG_IN_IMETOASCIIEX             0x800000
#define MSG_IMN_DESTROYCAND             0x004000
#define MSG_BACKSPACE                   0x000800
#define GEN_MSG_MAX             6

// the flag for set context
#define SC_SHOW_UI              0x0001
#define SC_HIDE_UI              0x0002
#define SC_ALREADY_SHOW_STATUS  0x0004
#define SC_WANT_SHOW_STATUS     0x0008
#define SC_HIDE_STATUS          0x0010

// the new flag for set context
#define ISC_OPEN_STATUS_WINDOW  0x04000000
#define ISC_OFF_CARET_UI        0x08000000
#define ISC_SHOW_UI_ALL         (ISC_SHOWUIALL|ISC_OPEN_STATUS_WINDOW)
#define ISC_SETCONTEXT_UI       (ISC_SHOWUIALL)

#define ISC_HIDE_COMP_WINDOW            0x00400000
#define ISC_HIDE_CAND_WINDOW            0x00800000
// the flag for composition string show status
#define IME_STR_SHOWED          0x0001
#define IME_STR_ERROR           0x0002

#define VK_OEM_SEMICLN                  0xba	//  ;    :
#define VK_OEM_EQUAL                    0xbb	//  =    +
#define VK_OEM_SLASH                    0xbf	//  /    ?
#define VK_OEM_LBRACKET                 0xdb	//  [    {
#define VK_OEM_BSLASH                   0xdc	//  \    |
#define VK_OEM_RBRACKET                 0xdd	//  ]    }
#define VK_OEM_QUOTE                    0xde	//  '    "

#define MAXMBNUMS                       40

// for ime property info
#define MAXNUMCODES                     48

#define LINE_LEN                80
#define CLASS_LEN               24

#define NumsSK                  13

// mb file for create word
#define ID_LENGTH 28
#define NUMTABLES 7
#define TAG_DESCRIPTION           0x00000001
#define TAG_RULER                 0x00000002
#define TAG_CRTWORDCODE           0x00000004

#define CMENU_HUIWND            0
#define CMENU_MENU              4

#define WM_USER_DESTROY         (WM_USER + 0x0400)

#define NEAR_CARET_FIRST_TIME   0x0001
#define NEAR_CARET_CANDIDATE    0x0002

typedef DWORD UNALIGNED FAR *LPUNADWORD;
typedef WORD UNALIGNED FAR *LPUNAWORD;

typedef UINT u32;
typedef unsigned char u8;
typedef unsigned short u16;

typedef struct tagImeL {		// local structure, per IME structure
	HMENU hPropMenu;			// Property Menu
	HINSTANCE hInst;			// IME DLL instance handle
	int xCompWi;				// width
	int yCompHi;				// height
	int cxCompBorder;			// border width of composition window
	int cyCompBorder;			// border height of composition window
	// window key related data
	WORD fModeConfig;			// quick key/prediction mode
	WORD nMaxKey;				// max key of compsiton str
	DWORD dwRegImeIndex;		// this value defers in different
	// process, so can not set in sImeG
	BOOL fWinLogon;
} IMEL;

typedef IMEL *PIMEL;
typedef IMEL NEAR *NPIMEL;
typedef IMEL FAR *LPIMEL;


// global sturcture for ime init data
typedef struct _tagImeG {
	TCHAR UsedCodes[17];
	WORD wNumCodes;
	DWORD IC_Enter;

	int xChiCharWi;
	int yChiCharHi;

	int xStatusWi;				// width of status window
	int yStatusHi;				// high of status window
	RECT rcStatusText;			// text position relative to status window
	RECT rcImeIcon;				// ImeIcon position relative to status window
	RECT rcImeName;				// ImeName position relative to status window
	RECT rcSymbol;				// symbol relative to status window
	//RECT        rcSKText;       // SK text relative to status window
	TCHAR szStatusErr[8];
	int cbStatusErr;
	int iCandStart;
// setting of UI
	int iPara;
	int iPerp;
	int iParaTol;
	int iPerpTol;
} IMEG;


typedef IMEG *PIMEG;
typedef IMEG NEAR *NPIMEG;
typedef IMEG FAR *LPIMEG;


typedef struct _tagPRIVCONTEXT {	// IME private data for each context
	int iImeState;				// the composition state - input, choose, or
	BOOL fdwImeMsg;				// what messages should be generated
	DWORD dwCompChar;			// wParam of WM_IME_COMPOSITION
	DWORD fdwGcsFlag;			// lParam for WM_IME_COMPOSITION
	u32 uSYHFlg;
	u32 uDYHFlg;
	u32 uDSMHCount;
	u32 uDSMHFlg;
// input data
	TCHAR bSeq[13];				// sequence code of input char
	DWORD fdwGB;
} PRIVCONTEXT;

typedef PRIVCONTEXT *PPRIVCONTEXT;
typedef PRIVCONTEXT NEAR *NPPRIVCONTEXT;
typedef PRIVCONTEXT FAR *LPPRIVCONTEXT;

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
extern IMEL sImeL;
extern LPIMEL lpImeL;
extern HDC ST_UI_hDC;
extern u32 uCaps;
extern DWORD SaTC_Trace;
extern TCHAR szUIClassName[];
extern TCHAR szCompClassName[];
extern TCHAR szStatusClassName[];
extern TCHAR szCMenuClassName[];
extern TCHAR szHandCursor[];
extern TCHAR szChinese[];
extern TCHAR pszImeName[IMEINDEXNUM][MAX_PATH];
extern TCHAR *szImeName;
extern TCHAR szImeRegName[];
extern TCHAR szImeXGBName[];
extern TCHAR szSymbol[];
extern TCHAR szNoSymbol[];
extern TCHAR szEnglish[];
extern TCHAR szCode[];
extern TCHAR szEudc[];
extern TCHAR szNone[];
extern TCHAR szDigit[];
extern BYTE bUpper[];
extern WORD fMask[];
extern TCHAR szRegIMESetting[];
extern TCHAR szPerp[];
extern TCHAR szPara[];
extern TCHAR szPerpTol[];
extern TCHAR szParaTol[];
extern TCHAR szRegImeIndex[];
extern const NEARCARET ncUIEsc[], ncAltUIEsc[];
extern const POINT ptInputEsc[], ptAltInputEsc[];
extern BYTE VirtKey48Map[];
extern TCHAR szWarnTitle[];
extern TCHAR szErrorTitle[];

LRESULT CALLBACK UIWndProc(HWND, u32, WPARAM, LPARAM);	// ui.c
LRESULT PASCAL UIPaint(HWND);	// ui.c

// for engine
WORD PASCAL AsciiToGB(LPPRIVCONTEXT);
WORD PASCAL CharToHex(TCHAR);


class input_context;
void PASCAL AddCodeIntoCand(LPCANDIDATELIST, WORD);	// compose.c
void PASCAL CompWord(WORD, input_context&, LPCOMPOSITIONSTRING, LPPRIVCONTEXT, LPGUIDELINE);	// compose.c
u32 PASCAL Finalize(input_context&, LPCOMPOSITIONSTRING, LPPRIVCONTEXT, WORD);	// compose.c
void PASCAL CompEscapeKey(input_context&, LPCOMPOSITIONSTRING, LPGUIDELINE, LPPRIVCONTEXT);	// compose.c

void PASCAL SelectOneCand(input_context&, LPCOMPOSITIONSTRING, LPPRIVCONTEXT, LPCANDIDATELIST);	// chcand.c
void PASCAL CandEscapeKey(input_context&, LPPRIVCONTEXT);	// chcand.c
void PASCAL ChooseCand(WORD, input_context&, LPCANDIDATEINFO, LPPRIVCONTEXT);	// chcand.c

void PASCAL SetPrivateFileSetting(LPBYTE, int, DWORD, LPCTSTR);	// ddis.c

void PASCAL InitCompStr(LPCOMPOSITIONSTRING);	// ddis.c
BOOL PASCAL ClearCand(input_context&);	// ddis.c
VOID InfoMessage(HANDLE, WORD);	//ddis.c
VOID FatalMessage(HANDLE, WORD);	//ddis.c

void PASCAL GenerateMessage(HIMC, input_context&, LPPRIVCONTEXT);	// notify.c

DWORD PASCAL ReadingToPattern(LPCTSTR, BOOL);	// regword.c
void PASCAL ReadingToSequence(LPCTSTR, LPBYTE, BOOL);	// regword.c

extern "C" HWND PASCAL GetCompWnd(HWND);	// compui.c
void PASCAL SetCompPosition(HWND, HIMC, input_context&);	// compui.c
void PASCAL MoveDefaultCompPosition(HWND);	// compui.c
void PASCAL ShowComp(HWND, int);	// compui.c
void PASCAL StartComp(HWND);	// compui.c
void PASCAL EndComp(HWND);		// compui.c
LRESULT CALLBACK CompWndProc(HWND, u32, WPARAM, LPARAM);	// compui.c
void PASCAL CompCancel(HIMC, input_context&);

LRESULT PASCAL SetStatusWindowPos(HWND);	// statusui.c
void PASCAL ShowStatus(HWND, int);	// statusui.c
void PASCAL OpenStatus(HWND);	// statusui.c
LRESULT CALLBACK StatusWndProc(HWND, u32, WPARAM, LPARAM);
BOOL IsUsedCode(WORD);
void PASCAL InitStatusUIData(int, int);
BOOL UpdateStatusWindow(HWND);
void PASCAL GenerateImeMessage(HIMC, input_context&, DWORD);
u32 PASCAL TranslateSymbolChar(LPTRANSMSGLIST, WORD, BOOL);
u32 PASCAL UnicodeProcessKey(WORD kbd_char, LPPRIVCONTEXT imcPrivPtr);
WORD PASCAL UnicodeEngine(LPPRIVCONTEXT imcPrivPtr);
void PASCAL UnicodeAddCodeIntoCand(LPCANDIDATELIST, WORD);
// dialog procedure
const char* msg_name(u32 msg);
extern HWND hCompWnd, hStatusWnd;
#endif
