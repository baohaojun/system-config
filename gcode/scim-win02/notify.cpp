
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

/**********************************************************************/
/* GenerateMessage()                                                  */
/**********************************************************************/
void PASCAL GenerateMessage(
    HIMC           hIMC,
    LPINPUTCONTEXT lpIMC,
    LPPRIVCONTEXT  lpImcP)
{
    EnterLeaveDebug();

    if (!hIMC) {
        return;
    } else if (!lpIMC) {
        return;
    } else if (!lpImcP) {
        return;
    } else if (lpImcP->fdwImeMsg & MSG_IN_IMETOASCIIEX) {
        return;
    } else {
    }

    lpIMC->dwNumMsgBuf += TranslateImeMessage(NULL, lpIMC, lpImcP);

    lpImcP->fdwImeMsg &= (MSG_ALREADY_OPEN|MSG_ALREADY_START);
    lpImcP->fdwGcsFlag = 0;

    ImmGenerateMessage(hIMC);
    return;
}

/**********************************************************************/
/* GenerateImeMessage()                                               */
/**********************************************************************/
void PASCAL GenerateImeMessage(
    HIMC           hIMC,
    LPINPUTCONTEXT lpIMC,
    DWORD          fdwImeMsg)
{
    EnterLeaveDebug();
    LPPRIVCONTEXT lpImcP;

    lpImcP = (LPPRIVCONTEXT)ImmLockIMCC(lpIMC->hPrivate);
    if (!lpImcP) {
        return;
    }

    lpImcP->fdwImeMsg |= fdwImeMsg;

    if (fdwImeMsg & MSG_CLOSE_CANDIDATE) {
        lpImcP->fdwImeMsg &= ~(MSG_OPEN_CANDIDATE|MSG_CHANGE_CANDIDATE);
    } else if (fdwImeMsg & (MSG_OPEN_CANDIDATE|MSG_CHANGE_CANDIDATE)) {
        lpImcP->fdwImeMsg &= ~(MSG_CLOSE_CANDIDATE);
    }

    if (fdwImeMsg & MSG_END_COMPOSITION) {
        lpImcP->fdwImeMsg &= ~(MSG_START_COMPOSITION);
    } else if (fdwImeMsg & MSG_START_COMPOSITION) {
        lpImcP->fdwImeMsg &= ~(MSG_END_COMPOSITION);
    }

    GenerateMessage(hIMC, lpIMC, lpImcP);

    ImmUnlockIMCC(lpIMC->hPrivate);

    return;
}

/**********************************************************************/
/* CompCancel()                                                       */
/**********************************************************************/
void PASCAL CompCancel(
    HIMC            hIMC,
    LPINPUTCONTEXT  lpIMC)
{
    EnterLeaveDebug();
    LPPRIVCONTEXT lpImcP;

    if (!lpIMC->hPrivate) {
        return;
    }

    lpImcP = (LPPRIVCONTEXT)ImmLockIMCC(lpIMC->hPrivate);
    if (!lpImcP) {
        return;
    }

    lpImcP->fdwGcsFlag = (DWORD)0;

    if (lpImcP->fdwImeMsg & MSG_ALREADY_OPEN) {
        CandEscapeKey(lpIMC, lpImcP);
    } else if (lpImcP->fdwImeMsg & MSG_ALREADY_START) {
        LPCOMPOSITIONSTRING lpCompStr;

        lpCompStr = (LPCOMPOSITIONSTRING)ImmLockIMCC(lpIMC->hCompStr);
        if (!lpCompStr) {          
            ImmUnlockIMCC(lpIMC->hCompStr);
            ImmUnlockIMCC(lpIMC->hPrivate);
            return;
        }

        CompEscapeKey(lpIMC, lpCompStr, lpImcP);


        if (lpCompStr) {
            ImmUnlockIMCC(lpIMC->hCompStr);
        }
    } else {
        ImmUnlockIMCC(lpIMC->hPrivate);
        return;
    }

    GenerateMessage(hIMC, lpIMC, lpImcP);

    ImmUnlockIMCC(lpIMC->hPrivate);

    return;
}


/**********************************************************************/
/* ImeSetCompositionString()                                          */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL WINAPI ImeSetCompositionString(
    HIMC   hIMC,
    DWORD  dwIndex,
    LPVOID lpComp,
    DWORD  dwCompLen,
    LPVOID lpRead,
    DWORD  dwReadLen)
{
    EnterLeaveDebug();
    return FALSE;
}

/**********************************************************************/
/* ToggleSoftKbd()                                                    */
/**********************************************************************/
void PASCAL ToggleSoftKbd(
    HIMC            hIMC,
    LPINPUTCONTEXT  lpIMC)
{
    EnterLeaveDebug();
    LPPRIVCONTEXT lpImcP;

    lpImcP = (LPPRIVCONTEXT)ImmLockIMCC(lpIMC->hPrivate);
    if (!lpImcP) {
        return;
    }

    lpImcP->fdwImeMsg |= MSG_IMN_UPDATE_SOFTKBD;

    GenerateMessage(hIMC, lpIMC, lpImcP);

    ImmUnlockIMCC(lpIMC->hPrivate);

    return;
}

/**********************************************************************/
/* NotifySelectCand()                                                 */
/**********************************************************************/
void PASCAL NotifySelectCand( // app tell IME that one candidate string is
                              // selected (by mouse or non keyboard action
                              // - for example sound)
    HIMC            hIMC,
    LPINPUTCONTEXT  lpIMC,
    LPCANDIDATEINFO lpCandInfo,
    DWORD           dwIndex,
    DWORD           dwValue)
{
    EnterLeaveDebug();
    LPCANDIDATELIST     lpCandList;
    LPCOMPOSITIONSTRING lpCompStr;
    LPPRIVCONTEXT       lpImcP;

    if (!lpCandInfo) {
        return;
    }

    if (dwIndex >= lpCandInfo->dwCount) {
        // wanted candidate list is not created yet!
        return;
    } else if (dwIndex == 0) {
        if (lpIMC->fdwConversion & IME_CMODE_CHARCODE) {
            return;         // not implemented yet
        }
    }

    lpCandList = (LPCANDIDATELIST)
        ((LPBYTE)lpCandInfo + lpCandInfo->dwOffset[0]);

    // the selected value even more than the number of total candidate
    // strings, it is imposible. should be error of app
    if (dwValue >= lpCandList->dwCount) {
        return;
    }

    // app select this candidate string
    lpCandList->dwSelection = dwValue;

    lpCompStr = (LPCOMPOSITIONSTRING)ImmLockIMCC(lpIMC->hCompStr);
    if(!lpCompStr){
        return;
    }
    lpImcP = (LPPRIVCONTEXT)ImmLockIMCC(lpIMC->hPrivate);
    if(!lpCompStr){
        ImmUnlockIMCC(lpIMC->hCompStr);
        return;
    }

    // translate into message buffer
    SelectOneCand(lpIMC, lpCompStr, lpImcP, lpCandList);

    // let app generate those messages in its message loop
    GenerateMessage(hIMC, lpIMC, lpImcP);

    ImmUnlockIMCC(lpIMC->hPrivate);
    ImmUnlockIMCC(lpIMC->hCompStr);

    return;
}

/**********************************************************************/
/* NotifyIME()                                                        */
/* Return Value:                                                      */
/*      TRUE - successful, FALSE - failure                            */
/**********************************************************************/
BOOL WINAPI NotifyIME(
    HIMC        hIMC,
    DWORD       dwAction,
    DWORD       dwIndex,
    DWORD       dwValue)
{
    EnterLeaveDebug();
    LPINPUTCONTEXT lpIMC;
    DWORD          fdwImeMsg;
    BOOL           fRet;

    fRet = FALSE;

    if (!hIMC) {
        return (fRet);
    }

    switch (dwAction) {
    case NI_OPENCANDIDATE:BHJDEBUG("case NI_OPENCANDIDATE");      // after a composition string is determined
        // if an IME can open candidate, it will.
        // if it can not, app also can not open it.
    case NI_CLOSECANDIDATE:BHJDEBUG("case NI_CLOSECANDIDATE");
        return (fRet);          // not supported
    case NI_SELECTCANDIDATESTR:BHJDEBUG("case NI_SELECTCANDIDATESTR");
    case NI_SETCANDIDATE_PAGESTART:BHJDEBUG("case NI_SETCANDIDATE_PAGESTART");
    case NI_SETCANDIDATE_PAGESIZE:BHJDEBUG("case NI_SETCANDIDATE_PAGESIZE");
        break;                  // need to handle it
    case NI_CHANGECANDIDATELIST:BHJDEBUG("case NI_CHANGECANDIDATELIST");
        break;
    case NI_CONTEXTUPDATED:BHJDEBUG("case NI_CONTEXTUPDATED");
        switch (dwValue) {
        case IMC_SETCONVERSIONMODE:BHJDEBUG("case IMC_SETCONVERSIONMODE");
        case IMC_SETOPENSTATUS:BHJDEBUG("case IMC_SETOPENSTATUS");
            break;              // need to handle it
        case IMC_SETCANDIDATEPOS:BHJDEBUG("case IMC_SETCANDIDATEPOS");
        case IMC_SETCOMPOSITIONFONT:BHJDEBUG("case IMC_SETCOMPOSITIONFONT");
        case IMC_SETCOMPOSITIONWINDOW:BHJDEBUG("case IMC_SETCOMPOSITIONWINDOW");
            return (TRUE);      // not important to the IME
        default:BHJDEBUG("default");
            return (fRet);      // not supported
        }
        break;
    case NI_COMPOSITIONSTR:BHJDEBUG("case NI_COMPOSITIONSTR");
        switch (dwIndex) {
        case CPS_COMPLETE:BHJDEBUG("case CPS_COMPLETE");
            break;              // need to handle it
        case CPS_CONVERT:BHJDEBUG("case CPS_CONVERT");       // all composition string can not be convert
        case CPS_REVERT:BHJDEBUG("case CPS_REVERT");        // any more, it maybe work for some
            // intelligent phonetic IMEs
            return (fRet);
        case CPS_CANCEL:BHJDEBUG("case CPS_CANCEL");
            break;              // need to handle it
        default:BHJDEBUG("default");
            return (fRet);      // not supported
        }
        break;                  // need to handle it
    default:BHJDEBUG("default");
        return (fRet);          // not supported
    }

    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {
        return (fRet);
    }

    fRet = TRUE;

    switch (dwAction) {
    case NI_CONTEXTUPDATED:BHJDEBUG("case NI_CONTEXTUPDATED");
        switch (dwValue) {
        case IMC_SETCONVERSIONMODE:BHJDEBUG("case IMC_SETCONVERSIONMODE");
            if ((lpIMC->fdwConversion ^ dwIndex) & IME_CMODE_CHARCODE) {
                // reject CHARCODE
                lpIMC->fdwConversion &= ~IME_CMODE_CHARCODE;
                MessageBeep((UINT)-1);
                break;
            }

            fdwImeMsg = 0;

            if ((lpIMC->fdwConversion ^ dwIndex) & IME_CMODE_NOCONVERSION) {
                lpIMC->fdwConversion |= IME_CMODE_NATIVE;
                lpIMC->fdwConversion &= ~(IME_CMODE_CHARCODE|
                                          IME_CMODE_EUDC|IME_CMODE_SYMBOL);
            }

            if ((lpIMC->fdwConversion ^ dwIndex) & IME_CMODE_EUDC) {
                lpIMC->fdwConversion |= IME_CMODE_NATIVE;
                lpIMC->fdwConversion &= ~(IME_CMODE_CHARCODE|
                                          IME_CMODE_NOCONVERSION|IME_CMODE_SYMBOL);
            }

            if ((lpIMC->fdwConversion ^ dwIndex) & IME_CMODE_SOFTKBD) {

                fdwImeMsg |= MSG_IMN_UPDATE_SOFTKBD;

            }

            if ((lpIMC->fdwConversion ^ dwIndex) == IME_CMODE_NATIVE) {
                lpIMC->fdwConversion &= ~(IME_CMODE_CHARCODE|
                                          IME_CMODE_NOCONVERSION|IME_CMODE_EUDC);
                fdwImeMsg |= MSG_IMN_UPDATE_SOFTKBD;
            }

            if (fdwImeMsg) {
                GenerateImeMessage(hIMC, lpIMC, fdwImeMsg);
            }

            if ((lpIMC->fdwConversion ^ dwIndex) & ~(IME_CMODE_FULLSHAPE|
                                                     IME_CMODE_SOFTKBD)) {
            } else {
                break;
            }

            CompCancel(hIMC, lpIMC);
            break;
        case IMC_SETOPENSTATUS:BHJDEBUG("case IMC_SETOPENSTATUS");
            if (lpIMC->fdwConversion & IME_CMODE_SOFTKBD) {
                GenerateImeMessage(hIMC, lpIMC, MSG_IMN_UPDATE_SOFTKBD);
            }
            CompCancel(hIMC, lpIMC);
            break;
        default:BHJDEBUG("default");
            break;
        }
        break;
    case NI_SELECTCANDIDATESTR:BHJDEBUG("case NI_SELECTCANDIDATESTR");
        if (!lpIMC->fOpen) {
            fRet = FALSE;
            break;
        } else if (lpIMC->fdwConversion & IME_CMODE_NOCONVERSION) {
            fRet = FALSE;
            break;
        } else if (lpIMC->fdwConversion & IME_CMODE_EUDC) {
            fRet = FALSE;
            break;
        } else if (!lpIMC->hCandInfo) {
            fRet = FALSE;
            break;
        } else {
            LPCANDIDATEINFO lpCandInfo;

            lpCandInfo = (LPCANDIDATEINFO)ImmLockIMCC(lpIMC->hCandInfo);
            if(!lpCandInfo){
                fRet = FALSE;
                break;
            }

            NotifySelectCand(hIMC, lpIMC, lpCandInfo, dwIndex, dwValue);

            ImmUnlockIMCC(lpIMC->hCandInfo);
        }

        break;
    case NI_CHANGECANDIDATELIST:BHJDEBUG("case NI_CHANGECANDIDATELIST");
        fdwImeMsg = 0;
        
        fdwImeMsg |= MSG_CHANGE_CANDIDATE;
        GenerateImeMessage(hIMC, lpIMC, fdwImeMsg);
        
        break;
    case NI_SETCANDIDATE_PAGESTART:BHJDEBUG("case NI_SETCANDIDATE_PAGESTART");
    case NI_SETCANDIDATE_PAGESIZE:BHJDEBUG("case NI_SETCANDIDATE_PAGESIZE");
        if (dwIndex != 0) {
            fRet = FALSE;
            break;
        } else if (!lpIMC->fOpen) {
            fRet = FALSE;
            break;
        } else if (lpIMC->fdwConversion & IME_CMODE_NOCONVERSION) {
            fRet = FALSE;
            break;
        } else if (lpIMC->fdwConversion & (IME_CMODE_EUDC|IME_CMODE_SYMBOL)) {
            fRet = FALSE;
            break;
        } else if (!lpIMC->hCandInfo) {
            fRet = FALSE;
            break;
        } else {
            LPCANDIDATEINFO lpCandInfo;
            LPCANDIDATELIST lpCandList;

            lpCandInfo = (LPCANDIDATEINFO)ImmLockIMCC(lpIMC->hCandInfo);
            if (!lpCandInfo) {
                fRet = FALSE;
                break;
            }

            lpCandList = (LPCANDIDATELIST)((LPBYTE)lpCandInfo +
                                           lpCandInfo->dwOffset[0]);

            if (dwAction == NI_SETCANDIDATE_PAGESTART) {
                if (dwValue < lpCandList->dwCount) {
                    lpCandList->dwPageStart = lpCandList->dwSelection =
                        dwValue;
                }
            } else {
                if (lpCandList->dwCount) {
                    lpCandList->dwPageSize = dwValue;
                }
            }

            ImmUnlockIMCC(lpIMC->hCandInfo);
        }

        break;
    case NI_COMPOSITIONSTR:BHJDEBUG("case NI_COMPOSITIONSTR");
        switch (dwIndex) {
        case CPS_CANCEL:BHJDEBUG("case CPS_CANCEL");
            CompCancel(hIMC, lpIMC);
            break;
        case CPS_COMPLETE:BHJDEBUG("case CPS_COMPLETE");
        {
            LPPRIVCONTEXT lpImcP;

            lpImcP = (LPPRIVCONTEXT)ImmLockIMCC(lpIMC->hPrivate);
            if (!lpImcP) {
                break;
            }

            if (lpImcP->iImeState == CST_INIT) {
                CompCancel(hIMC, lpIMC);
                // can not do any thing
            } else if (lpImcP->iImeState == CST_CHOOSE) {
                LPCOMPOSITIONSTRING lpCompStr;
                LPCANDIDATEINFO     lpCandInfo;

                lpCompStr = (LPCOMPOSITIONSTRING)ImmLockIMCC(lpIMC->hCompStr);
                if(!lpCompStr){
                    break;
                }


                lpCandInfo = (LPCANDIDATEINFO)ImmLockIMCC(lpIMC->hCandInfo);
                if (lpCandInfo) {
                    LPCANDIDATELIST lpCandList;

                    lpCandList = (LPCANDIDATELIST)((LPBYTE)lpCandInfo +
                                                   lpCandInfo->dwOffset[0]);

                    SelectOneCand(lpIMC, lpCompStr, lpImcP, lpCandList);

                    ImmUnlockIMCC(lpIMC->hCandInfo);

                    GenerateMessage(hIMC, lpIMC, lpImcP);
                }

                if (lpCompStr) ImmUnlockIMCC(lpIMC->hCompStr);
            } else if ((lpIMC->fdwConversion & (IME_CMODE_NATIVE|
                                                IME_CMODE_EUDC|IME_CMODE_SYMBOL)) != IME_CMODE_NATIVE) {
                CompCancel(hIMC, lpIMC);
            } else if (lpImcP->iImeState == CST_INPUT) {
                LPCOMPOSITIONSTRING lpCompStr;
                LPCANDIDATEINFO     lpCandInfo;

                lpCompStr = (LPCOMPOSITIONSTRING)ImmLockIMCC(lpIMC->hCompStr);
                if(!lpCompStr){
                    break;
                }

                CompWord(' ', lpIMC, lpCompStr, lpImcP);

                if (lpImcP->iImeState == CST_INPUT) {
                    CompCancel(hIMC, lpIMC);
                } else if (lpImcP->iImeState != CST_CHOOSE) {
                } else if (lpCandInfo = (LPCANDIDATEINFO)ImmLockIMCC(
                               lpIMC->hCandInfo)) {
                    LPCANDIDATELIST lpCandList;

                    lpCandList = (LPCANDIDATELIST)((LPBYTE)lpCandInfo +
                                                   lpCandInfo->dwOffset[0]);

//                        SelectOneCand(hIMC, lpIMC, lpCompStr, lpImcP, lpCandList);
                    SelectOneCand(lpIMC, lpCompStr, lpImcP, lpCandList);

                    ImmUnlockIMCC(lpIMC->hCandInfo);
                } else {
                }

                if (lpCompStr) ImmUnlockIMCC(lpIMC->hCompStr);

                // don't phrase predition under this case
                if (lpImcP->fdwImeMsg & MSG_ALREADY_OPEN) {
                    lpImcP->fdwImeMsg = (lpImcP->fdwImeMsg | MSG_CLOSE_CANDIDATE) &
                        ~(MSG_OPEN_CANDIDATE|MSG_CHANGE_CANDIDATE);
                } else {
                    lpImcP->fdwImeMsg &= ~(MSG_CLOSE_CANDIDATE|MSG_OPEN_CANDIDATE);
                }

                GenerateMessage(hIMC, lpIMC, lpImcP);
            } else {
                CompCancel(hIMC, lpIMC);
            }

            ImmUnlockIMCC(lpIMC->hPrivate);
        }
        break;
        default:BHJDEBUG("default");
            break;
        }
        break;
    default:BHJDEBUG("default");
        break;
    }

    ImmUnlockIMC(hIMC);
    return (fRet);
}


//Local Variables: ***
//coding: gb2312 ***
//End: ***
