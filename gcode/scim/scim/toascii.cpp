#include <windows.h>
#include <immdev.h>
#include <imedefs.h>
#include "imewnd.h"


BOOL WINAPI ImeProcessKey(HIMC hIMC,
							 u32 vk, LPARAM lParam,
							 CONST LPBYTE lpbKeyState)
{
	char kbd_char = LOBYTE(vk);
	if (isprint(kbd_char)) {
		return true;
	}

	return false;
}


static u32 to_wm_char(
	u32 vk,
	u32 kbd_scan, 
	LPTRANSMSGLIST lpTransBuf, 
	PBYTE kbd_state
	)
{

	BYTE szAscii[4];

	int nChars = ToAscii(vk, kbd_scan, kbd_state,
					 (LPWORD) szAscii, 0);

	if (!nChars) {
		//BHJDEBUG(" ToAscii failed"); //but it's ok, we will return 0
	}

	LPTRANSMSG lpTransMsg = lpTransBuf->TransMsg;
	int i;
	for (i=0; i<nChars; i++) {

		lpTransMsg[i].message = WM_CHAR;
		lpTransMsg[i].wParam = szAscii[i];
		lpTransMsg[i].lParam = (kbd_scan << 16) | 1UL;
	}
	return i;
}

u32 WINAPI
ImeToAsciiEx(u32 vk,
			 u32 kbd_scan,
			 CONST LPBYTE lpbKeyState,
			 LPTRANSMSGLIST lpTransBuf, u32 fuState, HIMC hIMC)
{
	char kbd_char = (char)(vk);

	if (isalpha(kbd_char)) {
		g_comp_str.push_back((char)tolower(kbd_char));
	}

	input_context ic(hIMC, lpTransBuf->TransMsg, lpTransBuf->uMsgCount);

	if (!ic) {
		return to_wm_char(vk, kbd_scan, lpTransBuf, lpbKeyState);
	}

	ic.add_msg(WM_IME_STARTCOMPOSITION, 0, 0);
	ic.add_msg(WM_IME_COMPOSITION, 0, 0);
	int n = 3;
	int i;
	for (i=0; i<n; i++) {
		BHJDEBUG(" i is %d", i);
		if (!ic.add_msg(WM_CHAR, 0x9999+i, 1)) {
			BHJDEBUG(" Error: add message");
			break;
		}
	}
	return i+2;
}
