
/*++

  Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

  Module Name:

  compui.c

  ++*/


#include <windows.h>
#include <immdev.h>
#include "imedefs.h"
#include <regstr.h>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h"

/**********************************************************************/
/* GetCompWnd                                                         */
/* Return Value :                                                     */
/*      window handle of composition                                  */
/**********************************************************************/
HWND PASCAL GetCompWnd(
    HWND hUIWnd)                // UI window
{
    EnterLeaveDebug();
    HGLOBAL  hUIPrivate;
    LPUIPRIV lpUIPrivate;
    HWND     hCompWnd;

    hUIPrivate = (HGLOBAL)GetWindowLongPtr(hUIWnd, IMMGWLP_PRIVATE);
    if (!hUIPrivate) {          // can not darw candidate window
        return (HWND)NULL;
    }

    lpUIPrivate = (LPUIPRIV)GlobalLock(hUIPrivate);
    if (!lpUIPrivate) {         // can not draw candidate window
        return (HWND)NULL;
    }

    hCompWnd = lpUIPrivate->hCompWnd;

    GlobalUnlock(hUIPrivate);
    return (hCompWnd);
}




/**********************************************************************/
/* SetCompPosition()                                                  */
/**********************************************************************/
void PASCAL SetCompPosition(    // set the composition window position
    HWND           hCompWnd,
    HIMC           hIMC,
    LPINPUTCONTEXT lpIMC)
{
    EnterLeaveDebug();
    POINT ptWnd;
    POINT ptSTWPos = {0};
    HWND  hCandWnd;
    BOOL  fChange = FALSE;
    RECT  rcWorkArea;



    rcWorkArea = ImeMonitorWorkAreaFromWindow(lpIMC->hWnd);


    // the client coordinate position (0, 0) of composition window
    ptWnd.x = 0;
    ptWnd.y = 0;
    // convert to screen coordinates
    ClientToScreen(hCompWnd, &ptWnd);
    ptWnd.x -= lpImeL->cxCompBorder;
    ptWnd.y -= lpImeL->cyCompBorder;
    
    dsimple_debug(lpIMC->cfCompForm.dwStyle);

    dsimple_debug(sImeG.IC_Trace);
    if (!sImeG.IC_Trace) {
        int  Comp_CandWndLen;

        ImmGetStatusWindowPos(hIMC, (LPPOINT)&ptSTWPos);

        // reset status window for LINE_UI(FIX_UI)
        Comp_CandWndLen = 0;
        if(uStartComp) {
            Comp_CandWndLen += lpImeL->xCompWi + UI_MARGIN;
            if(uOpenCand) {
                Comp_CandWndLen += sImeG.xCandWi + UI_MARGIN;
            }
            if(ptSTWPos.x+sImeG.xStatusWi+Comp_CandWndLen>rcWorkArea.right) {
                ptSTWPos.x=rcWorkArea.right-sImeG.xStatusWi-Comp_CandWndLen;
            }

            SetWindowPos(GetStatusWnd(GetWindow(hCompWnd, GW_OWNER)), NULL,
                         (int)ptSTWPos.x, (int)ptSTWPos.y,
                         0, 0, SWP_NOACTIVATE|SWP_NOCOPYBITS|SWP_NOSIZE|SWP_NOZORDER);
            ImmSetStatusWindowPos(hIMC, (LPPOINT)&ptSTWPos);
        }

        ptWnd.x = ptSTWPos.x + sImeG.xStatusWi + UI_MARGIN;
        ptWnd.y = ptSTWPos.y;
        lpIMC->cfCompForm.ptCurrentPos = ptWnd;
        ScreenToClient(lpIMC->hWnd, &lpIMC->cfCompForm.ptCurrentPos);
        fChange = TRUE;

    } else if (lpIMC->cfCompForm.dwStyle & CFS_FORCE_POSITION) {
        POINT ptNew;            // new position of UI

        ptNew.x = lpIMC->cfCompForm.ptCurrentPos.x;
        ptNew.y = lpIMC->cfCompForm.ptCurrentPos.y;
        ClientToScreen((HWND)lpIMC->hWnd, &ptNew);
        if (ptWnd.x != ptNew.x) {
            ptWnd.x = ptNew.x;
            fChange = TRUE;
        }
        if (ptWnd.y != ptNew.y) {
            ptWnd.y = ptNew.y;
            fChange = TRUE;
        }
        if (fChange) {
            ptWnd.x -= lpImeL->cxCompBorder;
            ptWnd.y -= lpImeL->cyCompBorder;
        }
    } else if (lpIMC->cfCompForm.dwStyle != CFS_DEFAULT) {
        POINT ptNew;            // new position of UI

        ptNew.x = lpIMC->cfCompForm.ptCurrentPos.x;
        ptNew.y = lpIMC->cfCompForm.ptCurrentPos.y;

        ClientToScreen((HWND)lpIMC->hWnd, &ptNew);
        ptWnd.x=ptNew.x;
        ptWnd.y=ptNew.y+22;
        fChange = true;
    } else {
        POINT ptNew;            // new position of UI

        ImmGetStatusWindowPos(hIMC, (LPPOINT)&ptSTWPos);
        ptNew.x = ptSTWPos.x + sImeG.xStatusWi + UI_MARGIN;
        if((ptSTWPos.x + sImeG.xStatusWi + sImeG.xCandWi + lpImeL->xCompWi + 2 * UI_MARGIN) >=
           rcWorkArea.right) { 
            ptNew.x = ptSTWPos.x - lpImeL->xCompWi - UI_MARGIN;
        }
        ptNew.y = ptSTWPos.y;
        if (ptWnd.x != ptNew.x) {
            ptWnd.x = ptNew.x;
            fChange = TRUE;
        }

        if (ptWnd.y != ptNew.y) {
            ptWnd.y = ptNew.y;
            fChange = TRUE;
        }

        if (fChange) {
            lpIMC->cfCompForm.ptCurrentPos = ptNew;

            ScreenToClient(lpIMC->hWnd, &lpIMC->cfCompForm.ptCurrentPos);
        }
    }

    if (!fChange) {
        return;
    }
    SetWindowPos(hCompWnd, NULL,
                 ptWnd.x, ptWnd.y,
                 0, 0, SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOZORDER);

    hCandWnd = GetCandWnd(GetWindow(hCompWnd, GW_OWNER));

    if (!hCandWnd) {
        return;
    }


    // decide the position of candidate window by UI's position
    CalcCandPos(hIMC, GetWindow(hCompWnd, GW_OWNER), &ptWnd);

    ScreenToClient(lpIMC->hWnd, &ptWnd);

    lpIMC->cfCandForm[0].dwStyle = CFS_CANDIDATEPOS;
    lpIMC->cfCandForm[0].ptCurrentPos = ptWnd;

    if (!IsWindowVisible(hCandWnd)) {
        return;
    }

    PostMessage(hCandWnd, WM_IME_NOTIFY, IMN_SETCANDIDATEPOS, 0x0001);

    return;
}


/**********************************************************************/
/* SetCompWindow()                                                    */
/**********************************************************************/
void PASCAL SetCompWindow(              // set the position of composition window
    HWND hCompWnd)
{
    EnterLeaveDebug();
    HIMC           hIMC;
    LPINPUTCONTEXT lpIMC;
    HWND           hUIWnd;

    hUIWnd = GetWindow(hCompWnd, GW_OWNER);
    if (!hUIWnd) {
        return;
    }
    hIMC = (HIMC)GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
    if (!hIMC) {
        return;
    }

    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {
        return;
    }

    SetCompPosition(hCompWnd, hIMC, lpIMC);

    ImmUnlockIMC(hIMC);

    return;
}

/**********************************************************************/
/* MoveDefaultCompPosition()                                          */
/**********************************************************************/
void PASCAL MoveDefaultCompPosition(    // the default comp position
                                        // need to near the caret
    HWND hUIWnd)
{
    EnterLeaveDebug();
    HIMC           hIMC;
    LPINPUTCONTEXT lpIMC;
    HWND           hCompWnd;

    hIMC = (HIMC)GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
    if (!hIMC) {
        return;
    }

    hCompWnd = GetCompWnd(hUIWnd);
    if (!hCompWnd) {
        return;
    }

    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {
        return;
    }

    if (!(lpIMC->cfCompForm.dwStyle & CFS_FORCE_POSITION)) {
        SetCompPosition(hCompWnd, hIMC, lpIMC);
    }

    ImmUnlockIMC(hIMC);

    return;
}

/**********************************************************************/
/* ShowComp()                                                         */
/**********************************************************************/
void PASCAL ShowComp(           // Show the composition window
    HWND hUIWnd,
    int  nShowCompCmd)
{
    EnterLeaveDebug();
    HGLOBAL  hUIPrivate;
    LPUIPRIV lpUIPrivate;

    // show or hid the UI window
    hUIPrivate = (HGLOBAL)GetWindowLongPtr(hUIWnd, IMMGWLP_PRIVATE);
    if (!hUIPrivate) {
        return;
    }

    lpUIPrivate = (LPUIPRIV)GlobalLock(hUIPrivate);
    if (!lpUIPrivate) {
        return;
    }

    if (lpUIPrivate->nShowCompCmd == nShowCompCmd) {
        goto SwCompNoChange;
    }

    if (nShowCompCmd == SW_HIDE) {
        lpUIPrivate->fdwSetContext &= ~(ISC_HIDE_COMP_WINDOW);
    }

    if (lpUIPrivate->hCompWnd) {
        if(nShowCompCmd == SW_HIDE) {
            uStartComp = 0;
        } else {
            uStartComp = 1;
        }
        
        ShowWindow(lpUIPrivate->hCompWnd, nShowCompCmd);
        lpUIPrivate->nShowCompCmd = nShowCompCmd;
    }

SwCompNoChange:
    GlobalUnlock(hUIPrivate);
    return;
}

/**********************************************************************/
/* StartComp()                                                        */
/**********************************************************************/
void PASCAL StartComp(
    HWND hUIWnd)
{
    EnterLeaveDebug();
    HIMC           hIMC;
    HGLOBAL        hUIPrivate;
    LPINPUTCONTEXT lpIMC;
    LPUIPRIV       lpUIPrivate;

    hIMC = (HIMC)GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
    if (!hIMC) {           
        return;
    }

    hUIPrivate = (HGLOBAL)GetWindowLongPtr(hUIWnd, IMMGWLP_PRIVATE);
    if (!hUIPrivate) {     
        return;
    }

    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {          
        return;
    }

    lpUIPrivate = (LPUIPRIV)GlobalLock(hUIPrivate);
    if (!lpUIPrivate) {    // can not draw composition window
        ImmUnlockIMC(hIMC);
        return;
    }

    lpUIPrivate->fdwSetContext |= ISC_SHOWUICOMPOSITIONWINDOW;

    if(!lpUIPrivate->hCompWnd) {
        lpUIPrivate->hCompWnd = CreateWindowEx(
            WS_EX_WINDOWEDGE|WS_EX_DLGMODALFRAME,
            szCompClassName, 
            NULL, 
            WS_POPUP|WS_DISABLED,
            0, 
            0, 
            lpImeL->xCompWi, 
            lpImeL->yCompHi,
            hUIWnd, 
            (HMENU)NULL, 
            hInst, 
            NULL);

        if ( lpUIPrivate->hCompWnd )
        {
            SetWindowLong(lpUIPrivate->hCompWnd, UI_MOVE_OFFSET, WINDOW_NOT_DRAG);
            SetWindowLong(lpUIPrivate->hCompWnd, UI_MOVE_XY, lpImeL->nMaxKey);
        }
    }

    uStartComp = 1;
    // try to set the position of composition UI window near the caret
    SetCompPosition(lpUIPrivate->hCompWnd, hIMC, lpIMC);

    ImmUnlockIMC(hIMC);

    ShowComp(hUIWnd, SW_SHOWNOACTIVATE);

    GlobalUnlock(hUIPrivate);

    return;
}

/**********************************************************************/
/* EndComp()                                                          */
/**********************************************************************/
void PASCAL EndComp(
    HWND hUIWnd)
{
    EnterLeaveDebug();
    ShowComp(hUIWnd, SW_HIDE);

    return;
}

/**********************************************************************/
/* DestroyCompWindow()                                                */
/**********************************************************************/
void PASCAL DestroyCompWindow(          // destroy composition window
    HWND hCompWnd)
{
    EnterLeaveDebug();
    HGLOBAL  hUIPrivate;
    LPUIPRIV lpUIPrivate;


    hUIPrivate = (HGLOBAL)GetWindowLongPtr(GetWindow(hCompWnd, GW_OWNER),
                                           IMMGWLP_PRIVATE);
    if (!hUIPrivate) {     
        return;
    }

    lpUIPrivate = (LPUIPRIV)GlobalLock(hUIPrivate);
    if (!lpUIPrivate) {    
        return;
    }

    lpUIPrivate->nShowCompCmd = SW_HIDE;

    lpUIPrivate->hCompWnd = (HWND)NULL;

    GlobalUnlock(hUIPrivate);
    return;
}



/**********************************************************************/
/* PaintCompWindow()                                                */
/**********************************************************************/
void PASCAL PaintCompWindow(
    HWND   hUIWnd,
    HWND   hCompWnd,
    HDC    hDC)
{
    EnterLeaveDebug();
    HIMC                hIMC;
    LPINPUTCONTEXT      lpIMC;
    HGDIOBJ             hOldFont;
    LPCOMPOSITIONSTRING lpCompStr;
    BOOL                fShowString;

    hIMC = (HIMC)GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
    if (!hIMC) {
        return;
    }

    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {
        return;
    }

    if (sImeG.fDiffSysCharSet) {
        LOGFONT lfFont;
        ZeroMemory(&lfFont, sizeof(lfFont));
        hOldFont = GetCurrentObject(hDC, OBJ_FONT);
        lfFont.lfHeight = -MulDiv(12, GetDeviceCaps(hDC, LOGPIXELSY), 72);
        lfFont.lfCharSet = NATIVE_CHARSET;
        lstrcpy(lfFont.lfFaceName, TEXT("Simsun"));
        SelectObject(hDC, CreateFontIndirect(&lfFont));
    }

    lpCompStr = (LPCOMPOSITIONSTRING)ImmLockIMCC(lpIMC->hCompStr);
    if (!lpCompStr) {          
        return;
    }

    
    SetBkColor(hDC, RGB(0xff, 0xff, 0xff));

    fShowString = (BOOL)0;

    if (fShowString & IME_STR_SHOWED) {
        // already show it, don't need to show
    } else if (lpCompStr->dwCompStrLen > 0) {
        ExtTextOut(hDC, lpImeL->rcCompText.left, lpImeL->rcCompText.top,
                   ETO_OPAQUE, &lpImeL->rcCompText,
                   (LPTSTR)NULL, 0, NULL);
        DrawText(hDC, (LPTSTR)((LPBYTE)lpCompStr + lpCompStr->dwCompStrOffset),
                 (int)lpCompStr->dwCompStrLen, &lpImeL->rcCompText,
                 DT_LEFT | DT_VCENTER | DT_SINGLELINE);
        if (fShowString & IME_STR_ERROR) {
            // red text for error


            SetBkColor(hDC, RGB(0x80, 0x80, 0x80));
            ExtTextOut(hDC, lpImeL->rcCompText.left +
                       lpCompStr->dwCursorPos * sImeG.xChiCharWi/ 2,
                       lpImeL->rcCompText.top,
                       ETO_CLIPPED, &lpImeL->rcCompText,
                       (LPTSTR)((LPBYTE)lpCompStr + lpCompStr->dwCompStrOffset +
                                lpCompStr->dwCursorPos),
                       (UINT)lpCompStr->dwCompStrLen - lpCompStr->dwCursorPos, NULL);
        } else if (lpCompStr->dwCursorPos < lpCompStr->dwCompStrLen) {
            // light gray background for cursor start
            SetBkColor(hDC, RGB(0x80, 0x80, 0x80));
            ExtTextOut(hDC, lpImeL->rcCompText.left +
                       lpCompStr->dwCursorPos * sImeG.xChiCharWi/ 2,
                       lpImeL->rcCompText.top,
                       ETO_CLIPPED, &lpImeL->rcCompText,
                       (LPTSTR)((LPBYTE)lpCompStr + lpCompStr->dwCompStrOffset +
                                lpCompStr->dwCursorPos),
                       (UINT)lpCompStr->dwCompStrLen - lpCompStr->dwCursorPos, NULL);
        } else {
        }
    } else {
        ExtTextOut(hDC, lpImeL->rcCompText.left, lpImeL->rcCompText.top,
                   ETO_OPAQUE, &lpImeL->rcCompText,
                   (LPTSTR)NULL, 0, NULL);
    }

    if (sImeG.fDiffSysCharSet) {
        DeleteObject(SelectObject(hDC, hOldFont));
    }

    ImmUnlockIMCC(lpIMC->hCompStr);
    ImmUnlockIMC(hIMC);
    return;
}

/**********************************************************************/
/* UpdateCompWindow()                                                 */
/**********************************************************************/
void PASCAL UpdateCompWindow(
    HWND hUIWnd)
{
    EnterLeaveDebug();
    HWND hCompWnd;
    HDC  hDC;

    hCompWnd = GetCompWnd(hUIWnd);
    hDC = GetDC(hCompWnd);
    PaintCompWindow(hUIWnd, hCompWnd, hDC);
    ReleaseDC(hCompWnd, hDC);
}

/**********************************************************************/
/* CompWndProc()                                                      */
/**********************************************************************/
LRESULT CALLBACK CompWndProc(           // composition window proc
    HWND   hCompWnd,
    UINT   uMsg,
    WPARAM wParam,
    LPARAM lParam)
{
    EnterLeaveDebug();
    switch (uMsg) {
    case WM_DESTROY:BHJDEBUG("case WM_DESTROY");
        DestroyCompWindow(hCompWnd);
        break;
    case WM_IME_NOTIFY:BHJDEBUG("case WM_IME_NOTIFY");
        if (wParam != IMN_SETCOMPOSITIONWINDOW) {
            // 9.8.del
            //} else if (sImeG.IC_Trace) {
            //    SetCompWindow(hCompWnd);
        } else {
            // 9.8.add
            SetCompWindow(hCompWnd);
        }
        break;
    case WM_PAINT:BHJDEBUG("case WM_PAINT");
    {
        HDC         hDC;
        PAINTSTRUCT ps;

        hDC = BeginPaint(hCompWnd, &ps);
        PaintCompWindow(GetWindow(hCompWnd, GW_OWNER), hCompWnd, hDC);
        EndPaint(hCompWnd, &ps);
    }
    break;
    case WM_MOUSEACTIVATE:BHJDEBUG("case WM_MOUSEACTIVATE");
        return (MA_NOACTIVATE);
    default:BHJDEBUG("default");
        return DefWindowProc(hCompWnd, uMsg, wParam, lParam);
    }
    return (0L);
}


//Local Variables: ***
//coding: gb2312 ***
//End: ***
