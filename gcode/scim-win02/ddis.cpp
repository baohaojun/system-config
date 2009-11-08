
/*++

  Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

  Module Name:

  ddis.c

  ++*/


#include <windows.h>
#include <windowsx.h>
#include <winerror.h>
#include <immdev.h>
#include <imedefs.h>
#include <resource.h>
#include <regstr.h>
#include <winuser.h>    
#define ENABLE_BHJDEBUG
#include "bhjdebug.h"

HWND  hCrtDlg = NULL;
/**********************************************************************/
/* ImeInquire()                                                       */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL WINAPI ImeInquire(         // initialized data structure of IME
    LPIMEINFO lpImeInfo,        // IME specific data report to IMM
    LPTSTR    lpszWndCls,       // the class name of UI
    DWORD     dwSystemInfoFlags)
{
    EnterLeaveDebug();
    if (!lpImeInfo) {
        return (FALSE);
    }

    lpImeInfo->dwPrivateDataSize = sizeof(PRIVCONTEXT);
    lpImeInfo->fdwProperty = IME_PROP_KBD_CHAR_FIRST|

        IME_PROP_UNICODE|

        IME_PROP_CANDLIST_START_FROM_1|
        IME_PROP_IGNORE_UPKEYS;

    lpImeInfo->fdwConversionCaps = IME_CMODE_NATIVE|IME_CMODE_FULLSHAPE|
        IME_CMODE_CHARCODE|IME_CMODE_SOFTKBD|IME_CMODE_NOCONVERSION;
    lpImeInfo->fdwSentenceCaps = 0;
    // IME will have different distance base multiple of 900 escapement
    lpImeInfo->fdwUICaps = UI_CAP_ROT90;
    // composition string is the reading string for simple IME
    lpImeInfo->fdwSCSCaps = SCS_CAP_COMPSTR|SCS_CAP_MAKEREAD;
    // IME want to decide conversion mode on ImeSelect
    lpImeInfo->fdwSelectCaps = (DWORD)0;

    lstrcpy(lpszWndCls, (LPTSTR)szUIClassName);

    if ( lpImeL )
    {
        if ( dwSystemInfoFlags & IME_SYSINFO_WINLOGON )
        {
            //  the client app is running in logon mode.
            lpImeL->fWinLogon = TRUE;
        }
        else
            lpImeL->fWinLogon = FALSE; 

    }

    return (TRUE);
}



/**********************************************************************/
/* ImeConfigure()                                                     */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
/*BOOL WINAPI ImeConfigure(      // configurate the IME setting
  HKL     hKL,               // hKL of this IME
  HWND    hAppWnd,           // the owner window
  DWORD   dwMode)            // mode of dialog
  {
  EnterLeaveDebug();*/
BOOL WINAPI ImeConfigure(      // configurate the IME setting
    HKL     hKL,               // hKL of this IME
    HWND    hAppWnd,           // the owner window
    DWORD   dwMode,
    LPVOID  lpData)            // mode of dialog
{
    EnterLeaveDebug();
    
    return (FALSE);
}

/**********************************************************************/
/* ImeConversionList()                                                */
/**********************************************************************/
DWORD WINAPI ImeConversionList(
    HIMC            hIMC,
    LPCTSTR         lpszSrc,
    LPCANDIDATELIST lpCandList,
    DWORD           uBufLen,
    UINT            uFlag)
{
    EnterLeaveDebug();
    return (0);
}

/**********************************************************************/
/* ImeDestroy()                                                       */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL WINAPI ImeDestroy(         // this dll is unloaded
    UINT uReserved)
{
    EnterLeaveDebug();
    if (uReserved) {
        return (FALSE);
    }

    return (TRUE);
}

/**********************************************************************/
/* ImeEscape()                                                        */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/

LRESULT WINAPI ImeEscape(       // escape function of IMEs
    HIMC   hIMC,
    UINT   uSubFunc,
    LPVOID lpData)
{
    EnterLeaveDebug();

    return FALSE;
}

/**********************************************************************/
/* InitCompStr()                                                      */
/**********************************************************************/
void PASCAL InitCompStr(                // init setting for composing string
    LPCOMPOSITIONSTRING lpCompStr)
{
    EnterLeaveDebug();
    if (!lpCompStr) {
        return;
    }

    lpCompStr->dwCompReadAttrLen = 0;
    lpCompStr->dwCompReadClauseLen = 0;
    lpCompStr->dwCompReadStrLen = 0;

    lpCompStr->dwCompAttrLen = 0;
    lpCompStr->dwCompClauseLen = 0;
    lpCompStr->dwCompStrLen = 0;

    lpCompStr->dwCursorPos = 0;
    lpCompStr->dwDeltaStart = 0;

    lpCompStr->dwResultReadClauseLen = 0;
    lpCompStr->dwResultReadStrLen = 0;

    lpCompStr->dwResultClauseLen = 0;
    lpCompStr->dwResultStrLen = 0;

    return;
}

/**********************************************************************/
/* ClearCompStr()                                                     */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL PASCAL ClearCompStr(
    LPINPUTCONTEXT lpIMC)
{
    EnterLeaveDebug();
    HIMCC               hMem;
    LPCOMPOSITIONSTRING lpCompStr;
    DWORD               dwSize;

    if(!lpIMC) {
        return (FALSE);
    }

    dwSize =
        // header length
        sizeof(COMPOSITIONSTRING) +
        // composition reading attribute plus NULL terminator
        lpImeL->nMaxKey * sizeof(BYTE) + sizeof(BYTE) +
        // composition reading clause
        sizeof(DWORD) + sizeof(DWORD) +
        // composition reading string plus NULL terminator
        lpImeL->nMaxKey * sizeof(WORD) + sizeof(WORD) +
        // result reading clause
        sizeof(DWORD) + sizeof(DWORD) +
        // result reading string plus NULL terminateor
        lpImeL->nMaxKey * sizeof(WORD) + sizeof(WORD) +
        // result clause
        sizeof(DWORD) + sizeof(DWORD) +
        // result string plus NULL terminateor
        MAXSTRLEN * sizeof(WORD) + sizeof(WORD);

    if (!lpIMC->hCompStr) {
        // it maybe free by other IME, init it
        lpIMC->hCompStr = ImmCreateIMCC(dwSize);
    } else if (hMem = ImmReSizeIMCC(lpIMC->hCompStr, dwSize)) {
        lpIMC->hCompStr = hMem;
    } else {
        ImmDestroyIMCC(lpIMC->hCompStr);
        lpIMC->hCompStr = ImmCreateIMCC(dwSize);
        return (FALSE);
    }

    if (!lpIMC->hCompStr) {
        return (FALSE);
    }

    lpCompStr = (LPCOMPOSITIONSTRING)ImmLockIMCC(lpIMC->hCompStr);
    if (!lpCompStr) {
        ImmDestroyIMCC(lpIMC->hCompStr);
        lpIMC->hCompStr = ImmCreateIMCC(dwSize);
        return (FALSE);
    }

    lpCompStr->dwSize = dwSize;

    // 1. composition (reading) string - simple IME
    // 2. result reading string
    // 3. result string

    lpCompStr->dwCompReadAttrLen = 0;
    lpCompStr->dwCompReadAttrOffset = sizeof(COMPOSITIONSTRING);
    lpCompStr->dwCompReadClauseLen = 0;
    lpCompStr->dwCompReadClauseOffset = lpCompStr->dwCompReadAttrOffset +
        lpImeL->nMaxKey * sizeof(TCHAR) + sizeof(TCHAR);
    lpCompStr->dwCompReadStrLen = 0;
    lpCompStr->dwCompReadStrOffset = lpCompStr->dwCompReadClauseOffset +
        sizeof(DWORD) + sizeof(DWORD);

    // composition string is the same with composition reading string 
    // for simple IMEs
    lpCompStr->dwCompAttrLen = 0;
    lpCompStr->dwCompAttrOffset = lpCompStr->dwCompReadAttrOffset;
    lpCompStr->dwCompClauseLen = 0;
    lpCompStr->dwCompClauseOffset = lpCompStr->dwCompReadClauseOffset;
    lpCompStr->dwCompStrLen = 0;
    lpCompStr->dwCompStrOffset = lpCompStr->dwCompReadStrOffset;

    lpCompStr->dwCursorPos = 0;
    lpCompStr->dwDeltaStart = 0;

    lpCompStr->dwResultReadClauseLen = 0;
    lpCompStr->dwResultReadClauseOffset = lpCompStr->dwCompStrOffset +
        lpImeL->nMaxKey * sizeof(WORD) + sizeof(WORD);
    lpCompStr->dwResultReadStrLen = 0;
    lpCompStr->dwResultReadStrOffset = lpCompStr->dwResultReadClauseOffset +
        sizeof(DWORD) + sizeof(DWORD);

    lpCompStr->dwResultClauseLen = 0;
    lpCompStr->dwResultClauseOffset = lpCompStr->dwResultReadStrOffset +
        lpImeL->nMaxKey * sizeof(WORD) + sizeof(WORD);
    lpCompStr->dwResultStrOffset = 0;
    lpCompStr->dwResultStrOffset = lpCompStr->dwResultClauseOffset +
        sizeof(DWORD) + sizeof(DWORD);

    GlobalUnlock((HGLOBAL)lpIMC->hCompStr);
    return (TRUE);
}

/**********************************************************************/
/* ClearCand()                                                        */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL PASCAL ClearCand(
    LPINPUTCONTEXT lpIMC)
{
    EnterLeaveDebug();
    HIMCC           hMem;
    LPCANDIDATEINFO lpCandInfo;
    LPCANDIDATELIST lpCandList;
    DWORD           dwSize =
        // header length
        sizeof(CANDIDATEINFO) + sizeof(CANDIDATELIST) +
        // candidate string pointers
        sizeof(DWORD) * (MAXCAND + 1) +
        // string plus NULL terminator
        (sizeof(WORD) + sizeof(WORD)) * (MAXCAND + 1);

    if (!lpIMC) {
        return (FALSE);
    }

    if (!lpIMC->hCandInfo) {
        // it maybe free by other IME, init it
        lpIMC->hCandInfo = ImmCreateIMCC(dwSize);
    } else if (hMem = ImmReSizeIMCC(lpIMC->hCandInfo, dwSize)) {
        lpIMC->hCandInfo = hMem;
    } else {
        ImmDestroyIMCC(lpIMC->hCandInfo);
        lpIMC->hCandInfo = ImmCreateIMCC(dwSize);
        return (FALSE);
    }

    if (!lpIMC->hCandInfo) {
        return (FALSE);
    } 

    lpCandInfo = (LPCANDIDATEINFO)ImmLockIMCC(lpIMC->hCandInfo);
    if (!lpCandInfo) {
        ImmDestroyIMCC(lpIMC->hCandInfo);
        lpIMC->hCandInfo = ImmCreateIMCC(dwSize);
        return (FALSE);
    }

    // ordering of strings are
    // buffer size
    lpCandInfo->dwSize = dwSize;
    lpCandInfo->dwCount = 0;
    lpCandInfo->dwOffset[0] = sizeof(CANDIDATEINFO);
    lpCandList = (LPCANDIDATELIST)((LPBYTE)lpCandInfo +
                                   lpCandInfo->dwOffset[0]);
    // whole candidate info size - header
    lpCandList->dwSize = lpCandInfo->dwSize - sizeof(CANDIDATEINFO);
    lpCandList->dwStyle = IME_CAND_READ;
    lpCandList->dwCount = 0;
    lpCandList->dwSelection = 0;
    lpCandList->dwPageSize = CANDPERPAGE;
    lpCandList->dwOffset[0] = sizeof(CANDIDATELIST) +
        sizeof(DWORD) * (MAXCAND );

    ImmUnlockIMCC(lpIMC->hCandInfo);
    return (TRUE);
}

/**********************************************************************/
/* InitContext()                                                      */
/**********************************************************************/
void PASCAL InitContext(
    LPINPUTCONTEXT lpIMC)
{
    EnterLeaveDebug();
    if (lpIMC->fdwInit & INIT_STATUSWNDPOS) {
    } else if (!lpIMC->hWnd) {
    } else {


        RECT rcWorkArea;

        rcWorkArea = ImeMonitorWorkAreaFromWindow(lpIMC->hWnd);

        lpIMC->ptStatusWndPos.x = rcWorkArea.left;

        lpIMC->ptStatusWndPos.y = rcWorkArea.bottom -
            sImeG.yStatusHi;


        lpIMC->fdwInit |= INIT_STATUSWNDPOS;
    }

    if (lpIMC->fdwInit & INIT_COMPFORM) {
    } else if (!lpIMC->hWnd) {
    } else {
        POINT ptWnd;

        ptWnd = lpImeL->ptDefComp;
        ScreenToClient(lpIMC->hWnd, &ptWnd);
        lpIMC->cfCompForm.dwStyle = CFS_DEFAULT;
        lpIMC->cfCompForm.ptCurrentPos = ptWnd;
        lpIMC->fdwInit |= INIT_COMPFORM;
    }

    return;
}

/**********************************************************************/
/* Select()                                                           */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL PASCAL Select(
    HIMC           hIMC,
    LPINPUTCONTEXT lpIMC,
    BOOL           fSelect)
{
    EnterLeaveDebug();
    LPPRIVCONTEXT  lpImcP;
    UINT           i;

    if (fSelect) {

        if (!ClearCompStr(lpIMC))
            return FALSE;

        if (!ClearCand(lpIMC))
            return FALSE;

    }

    if (lpIMC->cfCandForm[0].dwIndex != 0)
        lpIMC->cfCandForm[0].dwStyle = CFS_DEFAULT;

    // We add this hack for switching from other IMEs, this IME has a bug.
    // Before this bug fixed in this IME, it depends on this hack.
    if (lpIMC->cfCandForm[0].dwStyle == CFS_DEFAULT) {
        lpIMC->cfCandForm[0].dwIndex = (DWORD)-1;
    }

    if (!lpIMC->hPrivate)
        return FALSE;

    lpImcP = (LPPRIVCONTEXT)ImmLockIMCC(lpIMC->hPrivate);
    if (!lpImcP)
        return FALSE;

    if (fSelect) {
        //
        // init fields of hPrivate
        //
        lpImcP->iImeState  = CST_INIT;
        lpImcP->fdwImeMsg  = (DWORD)0;
        lpImcP->dwCompChar = (DWORD)0;
        lpImcP->fdwGcsFlag = (DWORD)0;

        lpImcP->uSYHFlg    = (UINT)0;
        lpImcP->uDYHFlg    = (UINT)0;
        lpImcP->uDSMHCount = (UINT)0;
        lpImcP->uDSMHFlg   = (UINT)0;

        //lpImcP->fdwSentence = (DWORD)NULL;

        //
        // reset SK State
        //

        *(LPDWORD)lpImcP->bSeq = 0;

        lpIMC->fOpen = TRUE;

        if (!(lpIMC->fdwInit & INIT_CONVERSION)) {
            lpIMC->fdwConversion = (lpIMC->fdwConversion & IME_CMODE_SOFTKBD) |
                IME_CMODE_NATIVE;
            lpIMC->fdwInit |= INIT_CONVERSION;
        }

        if (lpIMC->fdwInit & INIT_SENTENCE) {
        } else if (lpImeL->fModeConfig & MODE_CONFIG_PREDICT) {
            lpIMC->fdwSentence = IME_SMODE_PHRASEPREDICT;
            lpIMC->fdwInit |= INIT_SENTENCE;
        } 

        if (!(lpIMC->fdwInit & INIT_LOGFONT)) {
            HDC hDC;
            HGDIOBJ hSysFont;

            //hSysFont = GetStockObject(SYSTEM_FONT);
            hDC = GetDC(NULL);
            hSysFont = GetCurrentObject(hDC, OBJ_FONT);
            GetObject(hSysFont, sizeof(LOGFONT), &lpIMC->lfFont.A);
            ReleaseDC(NULL, hDC);

            lpIMC->fdwInit |= INIT_LOGFONT;
        }

        InitContext(lpIMC);

        //
        // Set Caps status
        //
        {
            DWORD fdwConversion;
        
            if (GetKeyState(VK_CAPITAL) & 0x01) {

                //
                // Change to alphanumeric mode.
                //
                fdwConversion = lpIMC->fdwConversion &
                    ~(IME_CMODE_NATIVE | IME_CMODE_CHARCODE | IME_CMODE_EUDC);
            } else {

                //
                // Change to native mode
                //
                fdwConversion = (lpIMC->fdwConversion | IME_CMODE_NATIVE) &
                    ~(IME_CMODE_CHARCODE | IME_CMODE_EUDC );
            }

            ImmSetConversionStatus(hIMC, fdwConversion, lpIMC->fdwSentence);
        }

    } else {

        if (lpImeL->hSKMenu) {
            DestroyMenu(lpImeL->hSKMenu);
            lpImeL->hSKMenu = NULL;
        }

        if (lpImeL->hPropMenu) {
            DestroyMenu(lpImeL->hPropMenu);
            lpImeL->hPropMenu = NULL;
        }

        if (hCrtDlg) {
            SendMessage(hCrtDlg, WM_CLOSE, (WPARAM)NULL, (LPARAM)NULL);
            hCrtDlg = NULL;
        }
    }

    ImmUnlockIMCC(lpIMC->hPrivate);

    return (TRUE);
}

/**********************************************************************/
/* ImeSelect()                                                        */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL WINAPI ImeSelect(
    HIMC hIMC,
    BOOL fSelect)
{
    EnterLeaveDebug();
    LPINPUTCONTEXT lpIMC;
    BOOL           fRet;


    if (!hIMC) {
        return (FALSE);
    }

    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {
        return (FALSE);
    }

    fRet = Select(hIMC, lpIMC, fSelect);

    ImmUnlockIMC(hIMC);

    return (fRet);
}

/**********************************************************************/
/* ImeSetActiveContext()                                              */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL WINAPI ImeSetActiveContext(
    HIMC   hIMC,
    BOOL   fOn)
{
    EnterLeaveDebug();
    if (!fOn) {
    } else if (!hIMC) {
    } else {
        LPINPUTCONTEXT lpIMC;

        lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
        if (!lpIMC) {
            return (FALSE);
        }

        InitContext(lpIMC);

        ImmUnlockIMC(hIMC);
    }

    return (TRUE);
}

//Local Variables: ***
//coding: gb2312 ***
//End: ***
