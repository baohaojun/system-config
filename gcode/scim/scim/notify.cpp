
/*++

Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

Module Name:

    notify.c


++*/


#include <windows.h>
#include <immdev.h>
#include <imedefs.h>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 
#include "imewnd.h"

//we don't allow ImeSetCompositionString
//we don't know what it does!

BOOL WINAPI
ImeSetCompositionString(HIMC hIMC,
						DWORD dwIndex,
						LPVOID lpComp,
						DWORD dwCompLen, LPVOID lpRead, DWORD dwReadLen)
{
	return FALSE;
}

BOOL WINAPI
NotifyIME(HIMC hIMC, DWORD dwAction, DWORD dwIndex, DWORD dwValue)
{
	return FALSE;
}
