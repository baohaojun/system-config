/*++

  Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

  Module Name:

  compose.c

  ++*/


#include <windows.h>
#include <immdev.h>
#include <imedefs.h>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h"

WORD PASCAL UnicodeEngine(LPPRIVCONTEXT lpImcP)
{
    EnterLeaveDebug();
    if (lpImcP->bSeq[3] || lpImcP->bSeq[2] == TEXT('?') || lpImcP->bSeq[2] == TEXT(' ')) {
        if (lpImcP->bSeq[2] == TEXT('?') || lpImcP->bSeq[2] == TEXT(' ')){
            lpImcP->bSeq[2] = TEXT('0');
            lpImcP->bSeq[3] = TEXT('0');
        }
        return (AsciiToGB(lpImcP));
    } else {
        return (0);
    }
}


/**********************************************************************/
/* AsciiToGB                                                          */
/* Description:                                                       */
/**********************************************************************/
WORD PASCAL AsciiToGB(LPPRIVCONTEXT lpImcP)
{
    EnterLeaveDebug();
    WORD GBCode;

    GBCode = (CharToHex(lpImcP->bSeq[2]) << 4) + CharToHex(lpImcP->bSeq[3]);
    GBCode = GBCode * 256;
    GBCode = (CharToHex(lpImcP->bSeq[0]) << 4) + CharToHex(lpImcP->bSeq[1]) + GBCode;

    return (GBCode);
}

/**********************************************************************/
/* AsciiToArea                                                        */
/* Description:                                                       */
/**********************************************************************/
WORD PASCAL AsciiToArea(LPPRIVCONTEXT lpImcP)
{
    EnterLeaveDebug();
    WORD AreaCode;
    AreaCode = (CharToHex(lpImcP->bSeq[2]) * 10) + CharToHex(lpImcP->bSeq[3]) + 0xa0;
    AreaCode = AreaCode * 256;
    AreaCode = (CharToHex(lpImcP->bSeq[0]) * 10) + CharToHex(lpImcP->bSeq[1]) + AreaCode + 0xa0;
    return (AreaCode);
}

WORD PASCAL CharToHex(
    TCHAR cChar)
{
    EnterLeaveDebug();
    if (cChar >= TEXT('0') && cChar <= TEXT('9'))
        return((WORD)(cChar - TEXT('0')));
    else if (cChar >= TEXT('a') && cChar <= TEXT('f'))
        return((WORD)(cChar-TEXT('a')+ 0x0a));
    else
        return ((WORD)NULL);
}



/**********************************************************************/
/* Engine()                                                           */
/* Description:                                                       */
/*      search MB and fill lpCompStr and lpCandList                   */
/**********************************************************************/
int PASCAL Engine(
    LPCOMPOSITIONSTRING lpCompStr,
    LPCANDIDATELIST     lpCandList,
    LPPRIVCONTEXT       lpImcP,
    LPINPUTCONTEXT      lpIMC,
    WORD                wCharCode)
{
    EnterLeaveDebug();

    if(lpCompStr->dwCursorPos < 4
       && (lpImcP->bSeq[2] != TEXT('?'))
       && (wCharCode != TEXT(' '))) {
        return (ENGINE_COMP);
    } else if((lpCompStr->dwCursorPos==4)
              ||(lpImcP->bSeq[2] == TEXT('?'))
              ||((wCharCode == TEXT(' ')) && (lpCompStr->dwCursorPos == 2))) {

        if (!lpCompStr) {
            MessageBeep((UINT)-1);
            return -1;
        }

        if (!lpImcP) {
            MessageBeep((UINT)-1);
            return -1;
        }


        if(sImeL.dwRegImeIndex == INDEX_UNICODE)
        {
            // UNICODE
            DWORD i;
            WORD wCode, xCode;
            TCHAR ResaultStr[3];

            memset(ResaultStr, 0, sizeof(ResaultStr));

            if((lpImcP->bSeq[2] == TEXT('?') || wCharCode == TEXT(' ')))  {
                lpImcP->bSeq[2] = TEXT('0');
                lpImcP->bSeq[3] = TEXT('0');
                lpImcP->bSeq[4] = TEXT('\0');

                wCode = UnicodeEngine(lpImcP);

                wCode = HIBYTE(wCode) | (LOBYTE(wCode) << 8);

                lpCandList->dwCount = 0;
                for (i = 0; i < IME_UNICODE_MAXCAND; i++, wCode++) {

                    // add this string into candidate list
                    *(LPTSTR)((LPBYTE)lpCandList + lpCandList->dwOffset[
                                  lpCandList->dwCount]) = wCode;

                    // null terminator
                    *(LPTSTR)((LPBYTE)lpCandList + lpCandList->dwOffset[
                                  lpCandList->dwCount] + sizeof(WORD)) = TEXT('\0');

                    lpCandList->dwOffset[lpCandList->dwCount + 1] =
                        lpCandList->dwOffset[lpCandList->dwCount] +
                        sizeof(WORD) + sizeof(TCHAR);
                    lpCandList->dwCount++;

                }
                return (ENGINE_COMP);
            } else {
                InitCompStr(lpCompStr);

                // the result string = the selected candidate;
                wCode = UnicodeEngine(lpImcP);
                {
                    WCHAR    UniStr[2];

                    UniStr[0] = HIBYTE(wCode) | (LOBYTE(wCode) << 8);
                    UniStr[1] = 0;
                    lstrcpy((LPTSTR)((LPBYTE)lpCompStr + lpCompStr->dwResultStrOffset),UniStr);

                    // calculate result string length
                    lpCompStr->dwResultStrLen = lstrlen(UniStr);
                }
                return (ENGINE_RESAULT);
            }
        }

    }
    MessageBeep((UINT)-1);
    return (ENGINE_COMP);
}


/**********************************************************************/
/* CompEscapeKey()                                                    */
/**********************************************************************/
void PASCAL CompEscapeKey(
    LPINPUTCONTEXT      lpIMC,
    LPCOMPOSITIONSTRING lpCompStr,
    LPPRIVCONTEXT       lpImcP)
{
    EnterLeaveDebug();

    if (lpImcP->iImeState != CST_CHOOSE) {
        if (lpImcP->fdwImeMsg & MSG_ALREADY_START) {
            lpImcP->fdwImeMsg = (lpImcP->fdwImeMsg | MSG_END_COMPOSITION) &
                ~(MSG_START_COMPOSITION);
        }
    }


    lpImcP->iImeState = CST_INIT;
    *(LPDWORD)lpImcP->bSeq = 0;

    if (lpCompStr) {
        InitCompStr(lpCompStr);
        lpImcP->fdwImeMsg |= MSG_COMPOSITION;
        lpImcP->dwCompChar = VK_ESCAPE;
        lpImcP->fdwGcsFlag |= (GCS_COMPREAD|GCS_COMP|GCS_CURSORPOS|
                               GCS_DELTASTART);
    }

    return;
}

/**********************************************************************/
/* CompBackSpaceKey()                                                 */
/**********************************************************************/
void PASCAL CompBackSpaceKey(
    LPINPUTCONTEXT      lpIMC,
    LPCOMPOSITIONSTRING lpCompStr,
    LPPRIVCONTEXT       lpImcP)
{
    EnterLeaveDebug();

    if (lpCompStr->dwCursorPos < sizeof(BYTE)) {
        lpCompStr->dwCursorPos = sizeof(BYTE);
    }

    lpImcP->bSeq[3] = 0;

    // go back a compsoition char
    lpCompStr->dwCursorPos -= sizeof(BYTE);

    // clean the sequence code
    lpImcP->bSeq[lpCompStr->dwCursorPos] = 0;

    lpImcP->fdwImeMsg |= MSG_COMPOSITION;
    lpImcP->dwCompChar = TEXT('\b');
    lpImcP->fdwGcsFlag |= (GCS_COMPREAD|GCS_COMP|GCS_CURSORPOS|
                           GCS_DELTASTART);

    if (!lpCompStr->dwCursorPos) {
        if (lpImcP->fdwImeMsg & (MSG_ALREADY_OPEN)) {
            ClearCand(lpIMC);
            lpImcP->fdwImeMsg = (lpImcP->fdwImeMsg | MSG_CLOSE_CANDIDATE) &
                ~(MSG_OPEN_CANDIDATE);
        }

        if(lpImcP->iImeState != CST_INIT) {
            lpImcP->iImeState = CST_INIT;
            lpCompStr->dwCompReadStrLen = lpCompStr->dwCompStrLen =
                lpCompStr->dwDeltaStart = lpCompStr->dwCursorPos;
            Finalize(lpIMC, lpCompStr, lpImcP, TEXT('\b'));
            return;
        }

        if (lpImcP->fdwImeMsg & MSG_ALREADY_START) {
            InitCompStr(lpCompStr);
            lpImcP->fdwImeMsg = (lpImcP->fdwImeMsg | MSG_END_COMPOSITION) &
                ~(MSG_START_COMPOSITION);
            return;
        }
    }

    // reading string is composition string for some simple IMEs
    // delta start is the same as cursor position for backspace
    lpCompStr->dwCompReadStrLen = lpCompStr->dwCompStrLen =
        lpCompStr->dwDeltaStart = lpCompStr->dwCursorPos;

    Finalize(lpIMC, lpCompStr, lpImcP, TEXT('\b'));

    return;
}

/**********************************************************************/
/* CompStrInfo()                                                      */
/**********************************************************************/
void PASCAL CompStrInfo(
    LPCOMPOSITIONSTRING lpCompStr,
    LPPRIVCONTEXT       lpImcP,
    WORD                wCharCode)
{
    EnterLeaveDebug();
    register DWORD dwCursorPos;

    //
    dwCursorPos = lpCompStr->dwCursorPos;

    // dwCrusorPos limit
    if (dwCursorPos >= lpImeL->nMaxKey) {
        // exceed the max input key limitation
        return;
    }

    // set MSG_START_COMPOSITION
    if (!(lpImcP->fdwImeMsg & MSG_ALREADY_START)) {
        lpImcP->fdwImeMsg = (lpImcP->fdwImeMsg | MSG_START_COMPOSITION) &
            ~(MSG_END_COMPOSITION);
    }

    if (lpImcP->iImeState == CST_INIT) {
        // clean the 4 bytes in one time
        *(LPDWORD)lpImcP->bSeq = 0;
    }


    lpImcP->bSeq[dwCursorPos] = (BYTE)wCharCode;

    // composition/reading string - UsedCode(Full Shape)
    lpImcP->dwCompChar = (DWORD)wCharCode;

    // set reading string for lpCompStr
    *((LPUNAWORD)((LPBYTE)lpCompStr + lpCompStr->dwCompReadStrOffset +
                  dwCursorPos*sizeof(TCHAR))) = (BYTE)lpImcP->dwCompChar;

    *((LPUNAWORD)((LPBYTE)lpCompStr + lpCompStr->dwCompReadAttrOffset +
                  dwCursorPos*sizeof(TCHAR))) = ((ATTR_TARGET_CONVERTED << 8)|ATTR_TARGET_CONVERTED);

    // set reading string lenght for lpCompStr
    if (lpCompStr->dwCompReadStrLen <= dwCursorPos) {
        lpCompStr->dwCompReadStrLen += sizeof(BYTE);
    }

    // composition string is reading string for some simple IMEs
    lpCompStr->dwCompStrLen = lpCompStr->dwCompReadStrLen;

    // composition/reading attribute length is equal to reading string length
    lpCompStr->dwCompReadAttrLen = lpCompStr->dwCompReadStrLen;
    lpCompStr->dwCompAttrLen = lpCompStr->dwCompStrLen;

    // delta start from previous cursor position
    lpCompStr->dwDeltaStart = lpCompStr->dwCursorPos;

    // set new cursor with next to the composition string
    lpCompStr->dwCursorPos = lpCompStr->dwCompStrLen;

    // set lpImcp->iImeState
    lpImcP->iImeState = CST_INPUT;

    // tell app, there is a composition char generated
    lpImcP->fdwImeMsg |= MSG_COMPOSITION;

    // set lpImeP->fdwGcsFlag
    lpImcP->fdwGcsFlag |= GCS_COMPREAD|GCS_COMP|GCS_CURSORPOS|GCS_DELTASTART;

    return;
}

/**********************************************************************/
/* Finalize()                                                         */
/* Return vlaue                                                       */
/*      Engine Flag                                                   */
/* Description:                                                       */
/*      Call Engine finalize Chinese word(s) by searching table       */
/*      (Set lpCompStr and lpCandList)                                */
/*      Set lpImeP(iImeState, fdwImeMsg, fdwGcsFlag)                  */
/**********************************************************************/
UINT PASCAL Finalize(
    LPINPUTCONTEXT      lpIMC,
    LPCOMPOSITIONSTRING lpCompStr,
    LPPRIVCONTEXT       lpImcP,
    WORD                wCharCode)
{
    EnterLeaveDebug();
    LPCANDIDATEINFO lpCandInfo;
    LPCANDIDATELIST lpCandList;
    UINT            fEngine;

    if (!lpIMC->hCandInfo) {
        return (0);
    }

    // get lpCandInfo
    lpCandInfo = (LPCANDIDATEINFO)ImmLockIMCC(lpIMC->hCandInfo);

    if (!lpCandInfo) {
        return (0);
    }

    // get lpCandList and init dwCount & dwSelection
    lpCandList = (LPCANDIDATELIST)
        ((LPBYTE)lpCandInfo + lpCandInfo->dwOffset[0]);
    lpCandList->dwCount = 0;
    lpCandList->dwSelection = 0;

    // search the IME tables
    fEngine =Engine(lpCompStr, lpCandList, lpImcP, lpIMC, wCharCode);

    if (fEngine == ENGINE_COMP) {
        lpCandInfo->dwCount  = 1;

        if (((lpCompStr->dwCursorPos < 3) && (wCharCode != TEXT(' ')))
            || ((lpCompStr->dwCursorPos == 3)
                && (wCharCode != TEXT(' ')) && (wCharCode != TEXT('?')))) {
            lpImcP->fdwImeMsg = (lpImcP->fdwImeMsg | MSG_CLOSE_CANDIDATE) &
                ~(MSG_OPEN_CANDIDATE);
            ImmUnlockIMCC(lpIMC->hCandInfo);
            return (fEngine);
        }

        if(lpCandList->dwCount != 0x0000) {
            // open composition candidate UI window for the string(s)
            if ((lpImcP->fdwImeMsg & (MSG_ALREADY_OPEN|MSG_CLOSE_CANDIDATE)) ==
                (MSG_ALREADY_OPEN|MSG_CLOSE_CANDIDATE)) {
                lpImcP->fdwImeMsg = (lpImcP->fdwImeMsg | MSG_CHANGE_CANDIDATE) &
                    ~(MSG_CLOSE_CANDIDATE);
            } else if (lpImcP->fdwImeMsg & MSG_ALREADY_OPEN) {
                lpImcP->fdwImeMsg |= MSG_CHANGE_CANDIDATE;
            } else {
                lpImcP->fdwImeMsg = (lpImcP->fdwImeMsg | MSG_OPEN_CANDIDATE) &
                    ~(MSG_CLOSE_CANDIDATE);
            }

        }

        if (lpImcP->fdwImeMsg & MSG_ALREADY_START) {
            lpImcP->fdwImeMsg |= MSG_COMPOSITION;
        }
    } else if (fEngine == ENGINE_ASCII) {
    } else if (fEngine == ENGINE_RESAULT) {

        // Set lpImep!   and tell application, there is a reslut string
        lpImcP->fdwImeMsg |= MSG_COMPOSITION;
        lpImcP->dwCompChar = (DWORD)0;
        lpImcP->fdwGcsFlag |= GCS_COMPREAD|GCS_COMP|GCS_CURSORPOS|
            GCS_DELTASTART|GCS_RESULTREAD|GCS_RESULT;

        if (lpImcP->fdwImeMsg & MSG_ALREADY_OPEN) {
            lpImcP->fdwImeMsg = (lpImcP->fdwImeMsg | MSG_CLOSE_CANDIDATE) &
                ~(MSG_OPEN_CANDIDATE);
        }
        // clear  candidate now
        lpCandList->dwCount = 0;
        // set iImeState with CST_INIT
        lpImcP->iImeState = CST_INIT;
        *(LPDWORD)lpImcP->bSeq = 0;
    }

    ImmUnlockIMCC(lpIMC->hCandInfo);

    return fEngine;
}

/**********************************************************************/
/* CompWord()                                                         */
/**********************************************************************/
void PASCAL CompWord(           // compose the Chinese word(s) according to
                                // input key
    WORD                wCharCode,
    LPINPUTCONTEXT      lpIMC,
    LPCOMPOSITIONSTRING lpCompStr,
    LPPRIVCONTEXT       lpImcP
    )
{
    EnterLeaveDebug();

    // lpComStr=NULL?
    if (!lpCompStr) {
        MessageBeep((UINT)-1);
        return;
    }

    // escape key
    if (wCharCode == VK_ESCAPE) {       // not good to use VK as char, but...
        CompEscapeKey(lpIMC, lpCompStr, lpImcP);
        return;
    }

    // backspace key
    if (wCharCode == TEXT('\b')) {
        CompBackSpaceKey(lpIMC, lpCompStr, lpImcP);
        return;
    }


    if(wCharCode == TEXT(' ')) {
    } else {
        // build up composition string info
        CompStrInfo(lpCompStr, lpImcP, wCharCode);
    }

    Finalize(lpIMC, lpCompStr, lpImcP, wCharCode);    // compsition

    return;
}

//Local Variables: ***
//coding: gb2312 ***
//End: ***
