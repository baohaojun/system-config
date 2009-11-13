#include <windows.h>
#include <immdev.h>
#include "imedefs.h"

IMEG sImeG = { 0 };

//#pragma data_seg()

HINSTANCE hInst;

TCHAR szUIClassName[CLASS_LEN] = L"BhjImeUI";
TCHAR szCompClassName[CLASS_LEN] = L"BhjImeComp";
TCHAR szStatusClassName[CLASS_LEN] = L"BhjImeStatus";
TCHAR *szImeName;




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


