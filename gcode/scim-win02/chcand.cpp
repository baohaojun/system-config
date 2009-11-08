/*++

  Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

  Module Name:

  chcand.c

  ++*/

#include <windows.h>
#include <immdev.h>
#include <imedefs.h>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h"



/**********************************************************************/
/* SelectOneCand()                                                    */
/**********************************************************************/
void PASCAL SelectOneCand(
    LPINPUTCONTEXT      lpIMC,
    LPCOMPOSITIONSTRING lpCompStr,
    LPPRIVCONTEXT       lpImcP,
    LPCANDIDATELIST     lpCandList)
{
    EnterLeaveDebug();

    DWORD dwCompStrLen;
    DWORD dwReadStrLen;

    if (!lpCompStr) {
        MessageBeep((UINT)-1);
        return;
    }

    if (!lpImcP) {
        MessageBeep((UINT)-1);
        return;
    }

    // backup the dwCompStrLen, this value decide whether
    // we go for phrase prediction
    dwCompStrLen = lpCompStr->dwCompStrLen;
    dwReadStrLen = lpCompStr->dwCompReadStrLen;

    InitCompStr(lpCompStr);

    // calculate result string length
    lpCompStr->dwResultStrLen = lstrlen(
        (LPTSTR)((LPBYTE)lpCandList + lpCandList->dwOffset[
                     lpCandList->dwSelection]));

    // the result string = the selected candidate;
    lstrcpy((LPTSTR)((LPBYTE)lpCompStr + lpCompStr->dwResultStrOffset),
            (LPTSTR)((LPBYTE)lpCandList + lpCandList->dwOffset[
                         lpCandList->dwSelection]));

    // tell application, there is a reslut string
    lpImcP->fdwImeMsg |= MSG_COMPOSITION;
    lpImcP->dwCompChar = (DWORD)0;
    lpImcP->fdwGcsFlag |= GCS_COMPREAD|GCS_COMP|GCS_CURSORPOS|
        GCS_DELTASTART|GCS_RESULTREAD|GCS_RESULT;

    if (lpImcP->fdwImeMsg & MSG_ALREADY_OPEN) {
        lpImcP->fdwImeMsg = (lpImcP->fdwImeMsg | MSG_CLOSE_CANDIDATE) &
            ~(MSG_OPEN_CANDIDATE);
    }

    // no candidate now, the right candidate string already be finalized
    lpCandList->dwCount = 0;

    lpImcP->iImeState = CST_INIT;
    
    
    // init Engine private data
    *(LPDWORD)lpImcP->bSeq = 0;

    return;
}

/**********************************************************************/
/* CandEscapeKey()                                                    */
/**********************************************************************/
void PASCAL CandEscapeKey(
    LPINPUTCONTEXT  lpIMC,
    LPPRIVCONTEXT   lpImcP)
{
    EnterLeaveDebug();

    LPCOMPOSITIONSTRING lpCompStr;

    // clean all candidate information
    if (lpImcP->fdwImeMsg & MSG_ALREADY_OPEN) {
        ClearCand(lpIMC);
        lpImcP->fdwImeMsg = (lpImcP->fdwImeMsg | MSG_CLOSE_CANDIDATE) &
            ~(MSG_OPEN_CANDIDATE);
    }


    // if it start composition, we need to clean composition
    if (!(lpImcP->fdwImeMsg & MSG_ALREADY_START)) {
        return;
    }

    lpCompStr = (LPCOMPOSITIONSTRING)ImmLockIMCC(lpIMC->hCompStr);
    if (!lpCompStr) {          
        return;
    }


    CompEscapeKey(lpIMC, lpCompStr, lpImcP);

    ImmUnlockIMCC(lpIMC->hCompStr);

    return;
}

/**********************************************************************/
/* ChooseCand()                                                       */
/**********************************************************************/
void PASCAL ChooseCand(         // choose one of candidate strings by
                                // input char
    WORD            wCharCode,
    LPINPUTCONTEXT  lpIMC,
    LPCANDIDATEINFO lpCandInfo,
    LPPRIVCONTEXT   lpImcP)
{
    EnterLeaveDebug();

    LPCANDIDATELIST     lpCandList;
    LPCOMPOSITIONSTRING lpCompStr;

    if (wCharCode == VK_ESCAPE) {           // escape key
        CandEscapeKey(lpIMC, lpImcP);
        return;
    }

    if (!lpCandInfo) {
        MessageBeep((UINT)-1);
        return;
    }

    lpCandList = (LPCANDIDATELIST)
        ((LPBYTE)lpCandInfo + lpCandInfo->dwOffset[0]);

    if (wCharCode == TEXT(' ')) {      // circle selection
        if ((lpCandList->dwSelection += lpCandList->dwPageSize) >=
            lpCandList->dwCount) {
            // no more candidates, restart it!
            lpCandList->dwSelection = 0;
            MessageBeep((UINT)-1);
        }
        // inform UI, dwSelectedCand is changed
        lpImcP->fdwImeMsg |= MSG_CHANGE_CANDIDATE;
        return;
    }

    if (wCharCode == TEXT('=')) {      // next selection

        if(sImeL.dwRegImeIndex == INDEX_UNICODE){
            if (lpCandList->dwSelection >= ((IME_UNICODE_MAXCAND-1)/CANDPERPAGE)*lpCandList->dwPageSize) {
                MessageBeep((UINT)-1);
                return;
            }
        }

        lpCandList->dwSelection += lpCandList->dwPageSize;
        // inform UI, dwSelectedCand is changed
        lpImcP->fdwImeMsg |= MSG_CHANGE_CANDIDATE;
        return;
    }

    if (wCharCode == TEXT('-')) {      // previous selection
        if (lpCandList->dwSelection < lpCandList->dwPageSize) {
            MessageBeep((UINT)-1);
            return;
        }
        lpCandList->dwSelection -= lpCandList->dwPageSize;
        // inform UI, dwSelectedCand is changed
        lpImcP->fdwImeMsg |= MSG_CHANGE_CANDIDATE;
        return;
    }

    if (wCharCode == 0x23) {      // previous selection

        if(sImeL.dwRegImeIndex == INDEX_UNICODE){
            if (lpCandList->dwSelection >= ((IME_UNICODE_MAXCAND-1)/CANDPERPAGE)*lpCandList->dwPageSize) {
                MessageBeep((UINT)-1);
                return;
            }else{
                lpCandList->dwSelection = ((IME_UNICODE_MAXCAND-1)/CANDPERPAGE)*lpCandList->dwPageSize;
            }
        }

        // inform UI, dwSelectedCand is changed
        lpImcP->fdwImeMsg |= MSG_CHANGE_CANDIDATE;
        return;
    }

    if (wCharCode == 0x24) {
        if (lpCandList->dwSelection < lpCandList->dwPageSize) {
            MessageBeep((UINT)-1);
            return;
        }
        lpCandList->dwSelection = 0;
        // inform UI, dwSelectedCand is changed
        lpImcP->fdwImeMsg |= MSG_CHANGE_CANDIDATE;
        return;
    }

    if (wCharCode == TEXT('?')) {      // home selection
        if (lpCandList->dwSelection == 0) {
            MessageBeep((UINT)-1);      // already home!
            return;
        }
        lpCandList->dwSelection = 0;
        // inform UI, dwSelectedCand is changed
        lpImcP->fdwImeMsg |= MSG_CHANGE_CANDIDATE;
        return;
    }

    if ((wCharCode >= TEXT('0')) && wCharCode <= TEXT('9')) {

        DWORD dwSelCand;

        dwSelCand = wCharCode - TEXT('0') - CAND_START;
        if(wCharCode == TEXT('0')) {
            dwSelCand = 9;
        }

        if (dwSelCand >= CANDPERPAGE) {
            // out of candidate page range
            MessageBeep((UINT)-1);
            return;
        }

        if ((lpCandList->dwSelection + dwSelCand) >=
            lpCandList->dwCount) {
            // out of range
            MessageBeep((UINT)-1);
            return;
        }

        lpCandList->dwSelection = lpCandList->dwSelection + dwSelCand;

        lpCompStr = (LPCOMPOSITIONSTRING)ImmLockIMCC(lpIMC->hCompStr);
        if (!lpCompStr) {          
            return;
        }

        // translate into translate buffer
        SelectOneCand(lpIMC, lpCompStr, lpImcP, lpCandList);

        ImmUnlockIMCC(lpIMC->hCompStr);

        return;
    }


    return;
}

//Local Variables: ***
//coding: gb2312 ***
//End: ***
