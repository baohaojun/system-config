
/*++

Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

Module Name:

    data.c

++*/


#include <windows.h>
#include <immdev.h>
#include "imedefs.h"

//#pragma comment(linker, "/SECTION:.shared,RWS")
//#pragma data_seg(".shared")
IMEG sImeG = { 0 };

//#pragma data_seg()

HDC ST_UI_hDC;
UINT uStartComp = 0;
UINT uCaps = 0;
DWORD SaTC_Trace;

HINSTANCE hInst;
IMEL sImeL;
LPIMEL lpImeL;
TCHAR szUIClassName[CLASS_LEN];
TCHAR szCompClassName[CLASS_LEN];
TCHAR szCandClassName[CLASS_LEN];
TCHAR szStatusClassName[CLASS_LEN];
TCHAR szCMenuClassName[CLASS_LEN];
TCHAR szHandCursor[] = TEXT("Hand");
TCHAR szChinese[] = TEXT("Chinese");
TCHAR szEnglish[] = TEXT("English");
TCHAR pszImeName[IMEINDEXNUM][MAX_PATH];
TCHAR *szImeName;
TCHAR szImeRegName[MAX_PATH];
TCHAR szCode[] = TEXT("Code");
TCHAR szEudc[] = TEXT("Eudc");
TCHAR szSymbol[] = TEXT("Symbol");
TCHAR szNoSymbol[] = TEXT("NoSymbol");
TCHAR szNone[] = TEXT("None");
TCHAR szDigit[] = TEXT("01234567890");

TCHAR szRegRevKL[] = { 0x7F16, 0x7801, 0x67E5, 0x8BE2, 0x0000 };
TCHAR szRegRevMaxKey[] =
	{ 0x7F16, 0x7801, 0x67E5, 0x8BE2, 0x7801, 0x957F, 0x0000 };
TCHAR szWarnTitle[] = { 0x8B66, 0x544A, 0x0000 };
TCHAR szErrorTitle[] = { 0x9519, 0x8BEF, 0x0000 };

// convert char to upper case
BYTE bUpper[] = {
// 0x20 - 0x27
	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
// 0x28 - 0x2F
	0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
// 0x30 - 0x37
	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
// 0x38 - 0x3F
	0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
// 0x40 - 0x47
	0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
// 0x48 - 0x4F
	0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
// 0x50 - 0x57
	0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
// 0x58 - 0x5F
	0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
//   '    a    b    c    d    e    f    g 
	'`', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
//   h    i    j    k    l    m    n    o
	'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
//   p    q    r    s    t    u    v    w
	'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
//   x    y    z    {    |    }    ~
	'X', 'Y', 'Z'
};

WORD fMask[] = {				// offset of bitfield
	0x0001, 0x0002, 0x0004, 0x0008, 0x0010, 0x0020, 0x0040, 0x0080,
	0x0100, 0x0200, 0x0400, 0x0800, 0x1000, 0x2000, 0x4000, 0x8000
};

TCHAR szRegIMESetting[] = TEXT("Control Panel\\Input Method");
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
