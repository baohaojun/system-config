#include <windows.h>
#include <immdev.h>
#include "imedefs.h"

IMEG sImeG = { 0 };

//#pragma data_seg()

HDC ST_UI_hDC;
u32 uCaps = 0;

HINSTANCE hInst;
TCHAR szUIClassName[CLASS_LEN];
TCHAR szCompClassName[CLASS_LEN];
TCHAR szCandClassName[CLASS_LEN];
TCHAR szStatusClassName[CLASS_LEN];
TCHAR szCMenuClassName[CLASS_LEN];
TCHAR szHandCursor[] = TEXT("Hand");
TCHAR szChinese[] = TEXT("Chinese");
TCHAR szEnglish[] = TEXT("English");
TCHAR *szImeName;
TCHAR szCode[] = TEXT("Code");
TCHAR szEudc[] = TEXT("Eudc");
TCHAR szSymbol[] = TEXT("Symbol");
TCHAR szNoSymbol[] = TEXT("NoSymbol");
TCHAR szNone[] = TEXT("None");

// convert char to upper case

TCHAR szPara[] = TEXT("Parallel Distance");
TCHAR szPerp[] = TEXT("Perpendicular Distance");
TCHAR szParaTol[] = TEXT("Parallel Tolerance");
TCHAR szPerpTol[] = TEXT("Perpendicular Tolerance");
TCHAR szRegImeIndex[] = TEXT("IME Index");
// decide UI offset base on escapement
const NEARCARET ncUIEsc[] = {
	// LogFontX  LogFontY  ParaX   PerpX   ParaY   PerpY
	{0, 1, 1, 0, 0, 1},			// 0
	{1, 0, 0, 1, 1, 0},			// 900
	{0, 0, -1, 0, 0, 1},		// 1800
	{-1, 0, 0, -1, -1, 0}		// 2700
};

// decide another UI offset base on escapement
const NEARCARET ncAltUIEsc[] = {
	// LogFontX  LogFontY  ParaX   PerpX   ParaY   PerpY
	{0, 0, 1, 0, 0, -1},		// 0
	{0, 0, 0, -1, 1, 0},		// 900
	{0, 0, -1, 0, 0, -1},		// 1800
	{0, 0, 0, 1, -1, 0}			// 2700
};

// decide input rectangle base on escapement
const POINT ptInputEsc[] = {
	// LogFontWi   LogFontHi
	{1, 1},						// 0
	{1, -1},					// 900
	{1, 1},						// 1800
	{-1, 1}						// 2700
};

// decide another input rectangle base on escapement
const POINT ptAltInputEsc[] = {
	// LogFontWi   LogFontHi
	{1, -1},					// 0
	{-1, -1},					// 900
	{1, -1},					// 1800
	{1, 1}						// 2700
};


BYTE VirtKey48Map[48] = {
	0x20, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,
	0x39, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
	0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0x53,
	0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0xba, 0xbb, 0xbc,
	0xbd, 0xbe, 0xbf, 0xc0, 0xdb, 0xdc, 0xdd, 0xde
};
