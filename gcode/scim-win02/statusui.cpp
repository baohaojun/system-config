
/*++

  Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

  Module Name:

  statusui.c


  ++*/


#include <windows.h>
#include <immdev.h>
#include <htmlhelp.h>
#include <string.h>
#include <regstr.h>
#include <imedefs.h>
#include <resource.h>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h"

extern HWND hCrtDlg;
/**********************************************************************/
/* GetStatusWnd                                                       */
/* Return Value :                                                     */
/*      window handle of status window                                */
/**********************************************************************/
HWND PASCAL GetStatusWnd(
    HWND hUIWnd)                // UI window
{
    EnterLeaveDebug();
    HGLOBAL  hUIPrivate;
    LPUIPRIV lpUIPrivate;
    HWND     hStatusWnd;

    hUIPrivate = (HGLOBAL)GetWindowLongPtr(hUIWnd, IMMGWLP_PRIVATE);
    if (!hUIPrivate) {          // can not darw status window
        return (HWND)NULL;
    }

    lpUIPrivate = (LPUIPRIV)GlobalLock(hUIPrivate);
    if (!lpUIPrivate) {         // can not draw status window
        return (HWND)NULL;
    }

    hStatusWnd = lpUIPrivate->hStatusWnd;

    GlobalUnlock(hUIPrivate);
    return (hStatusWnd);
}

/**********************************************************************/
/* AdjustStatusBoundary()                                             */
/**********************************************************************/
void PASCAL AdjustStatusBoundary(
    LPPOINTS lppt,
    HWND     hUIWnd)
{
    EnterLeaveDebug();

    RECT     rcWorkArea;


    {
        RECT rcStatusWnd;

        rcStatusWnd.left = lppt->x;
        rcStatusWnd.top = lppt->y;
        rcStatusWnd.right = rcStatusWnd.left + sImeG.xStatusWi;
        rcStatusWnd.bottom = rcStatusWnd.top + sImeG.yStatusHi;

        rcWorkArea = ImeMonitorWorkAreaFromRect(&rcStatusWnd);
    }


    // display boundary check
    if (lppt->x < rcWorkArea.left) {
        lppt->x = (short)rcWorkArea.left;
    } else if (lppt->x + sImeG.xStatusWi > rcWorkArea.right) {
        lppt->x = (short)(rcWorkArea.right - sImeG.xStatusWi);
    }

    if (lppt->y < rcWorkArea.top) {
        lppt->y = (short)rcWorkArea.top;
    } else if (lppt->y + sImeG.yStatusHi > rcWorkArea.bottom) {
        lppt->y = (short)(rcWorkArea.bottom - sImeG.yStatusHi);
    }

    if(sImeG.IC_Trace) {
    } else {
        int             Comp_CandWndLen;

        Comp_CandWndLen = 0;
        if(uStartComp) {
            Comp_CandWndLen += lpImeL->xCompWi + UI_MARGIN;
        }
        
        if(uOpenCand) {
            Comp_CandWndLen += sImeG.xCandWi + UI_MARGIN;
        }

        if(lppt->x + sImeG.xStatusWi + Comp_CandWndLen > rcWorkArea.right) {
            lppt->x=(SHORT)(rcWorkArea.right-sImeG.xStatusWi-Comp_CandWndLen);
        }
    }
    
    return;
}

/**********************************************************************/
/* SetStatusWindowPos()                                               */
/**********************************************************************/
LRESULT PASCAL SetStatusWindowPos(
    HWND   hStatusWnd)
{
    EnterLeaveDebug();
    HWND           hUIWnd;
    HIMC           hIMC;
    LPINPUTCONTEXT lpIMC;
    RECT           rcStatusWnd;
    POINTS         ptPos;

    hUIWnd = GetWindow(hStatusWnd, GW_OWNER);

    hIMC = (HIMC)GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
    if (!hIMC) {
        return (1L);
    }

    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {           // Oh! Oh!
        return (1L);
    }

    ptPos.x = (short)lpIMC->ptStatusWndPos.x;
    ptPos.y = (short)lpIMC->ptStatusWndPos.y;

    // display boundary adjust
    AdjustStatusBoundary(&ptPos, hUIWnd);

    SetWindowPos(hStatusWnd, NULL,
                 ptPos.x, ptPos.y,
                 0, 0, SWP_NOACTIVATE|SWP_NOCOPYBITS|SWP_NOSIZE|SWP_NOZORDER);

    ImmUnlockIMC(hIMC);

    return (0L);
}

/**********************************************************************/
/* ShowStatus()                                                       */
/**********************************************************************/
void PASCAL ShowStatus(         // Show the status window - shape / soft KBD
    // alphanumeric ...
    HWND hUIWnd,
    int  nShowStatusCmd)
{
    EnterLeaveDebug();
    HGLOBAL  hUIPrivate;
    LPUIPRIV lpUIPrivate;

    hUIPrivate = (HGLOBAL)GetWindowLongPtr(hUIWnd, IMMGWLP_PRIVATE);
    if (!hUIPrivate) {          // can not darw status window
        return;
    }

    lpUIPrivate = (LPUIPRIV)GlobalLock(hUIPrivate);
    if (!lpUIPrivate) {         // can not draw status window
        return;
    }

    if (!lpUIPrivate->hStatusWnd) {
        // not in show status window mode
    } else if (lpUIPrivate->nShowStatusCmd != nShowStatusCmd) {
        SystemParametersInfo(SPI_GETWORKAREA, 0, &sImeG.rcWorkArea, 0);
        SetStatusWindowPos(lpUIPrivate->hStatusWnd);
        ShowWindow(lpUIPrivate->hStatusWnd, nShowStatusCmd);
        lpUIPrivate->nShowStatusCmd = nShowStatusCmd;
    } else {
    }

    GlobalUnlock(hUIPrivate);
    return;
}

/**********************************************************************/
/* OpenStatus()                                                       */
/**********************************************************************/
void PASCAL OpenStatus(         // open status window
    HWND hUIWnd)
{
    EnterLeaveDebug();
    HGLOBAL        hUIPrivate;
    LPUIPRIV       lpUIPrivate;
    HIMC           hIMC;
    LPINPUTCONTEXT lpIMC;
    POINT          ptPos;
    int            nShowStatusCmd;
    RECT           rcWorkArea;

    rcWorkArea = sImeG.rcWorkArea;

    hUIPrivate = (HGLOBAL)GetWindowLongPtr(hUIWnd, IMMGWLP_PRIVATE);
    if (!hUIPrivate)           // can not darw status window
        return;
    

    lpUIPrivate = (LPUIPRIV)GlobalLock(hUIPrivate);
    if (!lpUIPrivate)          // can not draw status window
        return;
    

    hIMC = (HIMC)GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
    if (!hIMC) {
        ptPos.x = rcWorkArea.left;
        ptPos.y = rcWorkArea.bottom - sImeG.yStatusHi;
        nShowStatusCmd = SW_HIDE;
    } 
    else if (lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC)) {
           

        rcWorkArea = ImeMonitorWorkAreaFromWindow(lpIMC->hWnd);


        if (lpIMC->ptStatusWndPos.x < rcWorkArea.left) {
            lpIMC->ptStatusWndPos.x = rcWorkArea.left;
        } 
        else if (lpIMC->ptStatusWndPos.x+sImeG.xStatusWi>rcWorkArea.right) {
            lpIMC->ptStatusWndPos.x = rcWorkArea.right - sImeG.xStatusWi;
        }

        if (lpIMC->ptStatusWndPos.y < rcWorkArea.top) {
            lpIMC->ptStatusWndPos.y = rcWorkArea.top;
        } 
        else if (lpIMC->ptStatusWndPos.y+sImeG.yStatusHi>rcWorkArea.right) {
            lpIMC->ptStatusWndPos.y = rcWorkArea.bottom - sImeG.yStatusHi;
        }
    
        if(sImeG.IC_Trace) {
            ptPos.x = lpIMC->ptStatusWndPos.x;
            ptPos.y = lpIMC->ptStatusWndPos.y;
        } else {
            ptPos.x = rcWorkArea.left;
            ptPos.y = rcWorkArea.bottom - sImeG.yStatusHi;
        }

        ImmUnlockIMC(hIMC);
        nShowStatusCmd = SW_SHOWNOACTIVATE;
    } else {
        ptPos.x = rcWorkArea.left;
        ptPos.y = rcWorkArea.bottom - sImeG.yStatusHi;
        nShowStatusCmd = SW_HIDE;
    }

    if (lpUIPrivate->hStatusWnd) {
        SetWindowPos(lpUIPrivate->hStatusWnd, NULL,
                     ptPos.x, ptPos.y,
                     0, 0,
                     SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOZORDER);
    } else {                            // create status window
        lpUIPrivate->hStatusWnd = CreateWindowEx(
            WS_EX_WINDOWEDGE|WS_EX_DLGMODALFRAME,
            szStatusClassName, NULL, 
            WS_POPUP|WS_DISABLED,
            ptPos.x, ptPos.y,
            sImeG.xStatusWi, sImeG.yStatusHi,
            hUIWnd, (HMENU)NULL, hInst, NULL);

        if ( lpUIPrivate->hStatusWnd != NULL )
        {

            SetWindowLong(lpUIPrivate->hStatusWnd, UI_MOVE_OFFSET, WINDOW_NOT_DRAG);
            SetWindowLong(lpUIPrivate->hStatusWnd, UI_MOVE_XY, 0L);
        }
    }

    lpUIPrivate->fdwSetContext |= ISC_OPEN_STATUS_WINDOW;

    if (hIMC) {
        ShowStatus( hUIWnd, SW_SHOWNOACTIVATE);
    }

    GlobalUnlock(hUIPrivate);
    return;
}

/**********************************************************************/
/* DestroyStatusWindow()                                              */
/**********************************************************************/
void PASCAL DestroyStatusWindow(
    HWND hStatusWnd)
{
    EnterLeaveDebug();
    HWND     hUIWnd;
    HGLOBAL  hUIPrivate;
    LPUIPRIV lpUIPrivate;

    hUIWnd = GetWindow(hStatusWnd, GW_OWNER);

    hUIPrivate = (HGLOBAL)GetWindowLongPtr(hUIWnd, IMMGWLP_PRIVATE);
    if (!hUIPrivate) {          // can not darw status window
        return;
    }

    lpUIPrivate = (LPUIPRIV)GlobalLock(hUIPrivate);
    if (!lpUIPrivate) {         // can not draw status window
        return;
    }

    lpUIPrivate->nShowStatusCmd = SW_HIDE;

    lpUIPrivate->hStatusWnd = (HWND)NULL;

    GlobalUnlock(hUIPrivate);
    return;
}

/**********************************************************************/
/* SetStatus                                                          */
/**********************************************************************/
void PASCAL SetStatus(
    HWND    hStatusWnd,
    LPPOINT lpptCursor)
{
    EnterLeaveDebug();
    HWND           hUIWnd;
    HIMC           hIMC;
    LPINPUTCONTEXT lpIMC;

    hUIWnd = GetWindow(hStatusWnd, GW_OWNER);
    hIMC = (HIMC)GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
    if (!hIMC) {
        return;
    }

    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {
        return;
    }

    if (!lpIMC->fOpen) {
        ImmSetOpenStatus(hIMC, TRUE);
    } else if (PtInRect(&sImeG.rcImeIcon, *lpptCursor)) {
        DWORD fdwConversion;

        if (lpIMC->fdwConversion & (IME_CMODE_CHARCODE|IME_CMODE_EUDC)) {
            // change to native mode
            fdwConversion = (lpIMC->fdwConversion | IME_CMODE_NATIVE) &
                ~(IME_CMODE_CHARCODE | IME_CMODE_EUDC);
        } else if (lpIMC->fdwConversion & IME_CMODE_NATIVE) {
            // change to alphanumeric mode
            fdwConversion = lpIMC->fdwConversion & ~(IME_CMODE_CHARCODE |
                                                     IME_CMODE_NATIVE | IME_CMODE_EUDC);
        } else {

    
            BYTE  lpbKeyState[256];

            if ( !GetKeyboardState(lpbKeyState) )
            {
                ImmUnlockIMC(hIMC);
                return;
            }
    
            if (lpbKeyState[VK_CAPITAL] & 1)        
            {
                // Simulate a key press
                keybd_event( VK_CAPITAL,
                             0x3A,
                             KEYEVENTF_EXTENDEDKEY | 0,
                             0 );
 
                // Simulate a key release
                keybd_event( VK_CAPITAL,
                             0x3A,
                             KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP,
                             0);
            }
            fdwConversion = (lpIMC->fdwConversion | IME_CMODE_NATIVE) &
                ~(IME_CMODE_CHARCODE | IME_CMODE_EUDC);
            // 10.11 add
            uCaps = 0;
        }

        ImmSetConversionStatus(hIMC, fdwConversion, lpIMC->fdwSentence);
    } else if (PtInRect(&sImeG.rcImeName, *lpptCursor)) {

        DWORD dwConvMode;
        int     cxBorder, cyBorder;
        HKEY  hKeyCurrVersion;
        HKEY  hKeyGB;
        DWORD retCode;

        //change current IME index
        dwConvMode = lpIMC->fdwConversion ^ (IME_CMODE_INDEX_FIRST << sImeL.dwRegImeIndex);
        sImeL.dwRegImeIndex = (sImeL.dwRegImeIndex+1) % IMEINDEXNUM;
        szImeName = pszImeName[sImeL.dwRegImeIndex];
        dwConvMode |= (IME_CMODE_INDEX_FIRST << sImeL.dwRegImeIndex);

        // re-caculate statusuidata
        cxBorder = GetSystemMetrics(SM_CXBORDER);
        cyBorder = GetSystemMetrics(SM_CYBORDER);
        InitStatusUIData(cxBorder, cyBorder);

        ImmSetConversionStatus(hIMC, dwConvMode, lpIMC->fdwSentence);

    } else if (PtInRect(&sImeG.rcShapeText, *lpptCursor)) {
        DWORD dwConvMode;

        if (lpIMC->fdwConversion & IME_CMODE_CHARCODE) {
            MessageBeep((UINT)-1);
        } else if (lpIMC->fdwConversion & IME_CMODE_EUDC) {
            MessageBeep((UINT)-1);
        } else {
            dwConvMode = lpIMC->fdwConversion ^ IME_CMODE_FULLSHAPE;
            ImmSetConversionStatus(hIMC, dwConvMode, lpIMC->fdwSentence);
        }
    } else if (PtInRect(&sImeG.rcSymbol, *lpptCursor)) {
        DWORD fdwConversion;

        if (lpIMC->fdwConversion & IME_CMODE_CHARCODE) {
            MessageBeep((UINT)-1);
        } else {
            fdwConversion = lpIMC->fdwConversion ^ IME_CMODE_SYMBOL;
            ImmSetConversionStatus(hIMC, fdwConversion, lpIMC->fdwSentence);
        }
    } else if (PtInRect(&sImeG.rcSKText, *lpptCursor)) {
        DWORD fdwConversion;
        LPPRIVCONTEXT  lpImcP;

        lpImcP = (LPPRIVCONTEXT)ImmLockIMCC(lpIMC->hPrivate);
    
        if(lpImcP) {
            if(!(lpImeL->hSKMenu)) {
                lpImeL->hSKMenu = LoadMenu (hInst, TEXT("SKMENU"));
            }

            lpImeL->dwSKState[lpImeL->dwSKWant] = 
                lpImeL->dwSKState[lpImeL->dwSKWant]^1;
            fdwConversion = lpIMC->fdwConversion ^ IME_CMODE_SOFTKBD;
            ImmSetConversionStatus(hIMC, fdwConversion, lpIMC->fdwSentence);
            ImmUnlockIMCC(lpIMC->hPrivate);
        } else {
            MessageBeep((UINT)-1);
        }
    } else {
        MessageBeep((UINT)-1);
    }

    ImmUnlockIMC(hIMC);

    return;
}



/**********************************************************************/
/* PaintStatusWindow()                                                */
/**********************************************************************/
void PASCAL PaintStatusWindow(
    HDC  hDC,
    HWND hStatusWnd)
{
    EnterLeaveDebug();
    HWND           hUIWnd;
    HIMC           hIMC;
    LPINPUTCONTEXT lpIMC;
    LPPRIVCONTEXT  lpImcP;
    HGDIOBJ        hOldFont;
    HGDIOBJ        hImeIconBmp, hShapeBmp, hSymbolBmp, hSKBmp;
    HGDIOBJ        hOldBmp;
    HDC            hMemDC;

    hUIWnd = GetWindow(hStatusWnd, GW_OWNER);

    hIMC = (HIMC)GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
    if (!hIMC) {
        MessageBeep((UINT)-1);
        return;
    }

    if (!(lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC))) {
        MessageBeep((UINT)-1);
        return;
    }

    // get lpImcP
    if(!(lpImcP = (LPPRIVCONTEXT)ImmLockIMCC(lpIMC->hPrivate))) {
        MessageBeep((UINT)-1);
        return;
    }


    //in case the IME index has been changed and the ImeName size is different
    {
        POINTS         ptPos;

        ptPos.x = (short)lpIMC->ptStatusWndPos.x;
        ptPos.y = (short)lpIMC->ptStatusWndPos.y;

        SetWindowPos(hStatusWnd, NULL,
                     ptPos.x, ptPos.y,
                     sImeG.xStatusWi, sImeG.yStatusHi, 
                     SWP_NOACTIVATE|SWP_NOCOPYBITS|SWP_NOZORDER);
    }


    // set font
    if (sImeG.fDiffSysCharSet) {
        LOGFONT lfFont;

        ZeroMemory(&lfFont, sizeof(lfFont));
        hOldFont = GetCurrentObject(hDC, OBJ_FONT);
        lfFont.lfHeight = -MulDiv(12, GetDeviceCaps(hDC, LOGPIXELSY), 72);
        lfFont.lfCharSet = NATIVE_CHARSET;
        lstrcpy(lfFont.lfFaceName, TEXT("Simsun"));
        SelectObject(hDC, CreateFontIndirect(&lfFont));
    }

    // draw Ime Name

    if (lpIMC->fOpen) {
        SetTextColor(hDC, RGB(0x00, 0x00, 0x00));
    } else {
        SetTextColor(hDC, RGB(0x80, 0x80, 0x80));
    }

    SetBkColor(hDC, RGB(0xff, 0xff, 0xff));
    DrawText(hDC, szImeName, lstrlen(szImeName),
             &sImeG.rcImeName, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
    
    // load all bitmap
    hSymbolBmp = (HGDIOBJ)NULL;
    hShapeBmp = (HGDIOBJ)NULL;
    hSKBmp = (HGDIOBJ)NULL;

    if (!lpIMC->fOpen) {
        hSymbolBmp = LoadBitmap(hInst, szNone);
        hShapeBmp = LoadBitmap(hInst, szNone);
        hSKBmp = LoadBitmap(hInst, szNone);
        hImeIconBmp = LoadBitmap(hInst, szChinese);
    } else if (lpIMC->fdwConversion & IME_CMODE_NATIVE) {
        hImeIconBmp = LoadBitmap(hInst, szChinese);
    } else {
        hImeIconBmp = LoadBitmap(hInst, szEnglish);
    }

    if (!hShapeBmp) {
        if (lpIMC->fdwConversion & IME_CMODE_FULLSHAPE) {
            hShapeBmp = LoadBitmap(hInst, szFullShape);
        } else {
            hShapeBmp = LoadBitmap(hInst, szHalfShape);
        }
    }

    if (!hSymbolBmp) {
        if (lpIMC->fdwConversion & IME_CMODE_SYMBOL) {
            hSymbolBmp = LoadBitmap(hInst, szSymbol);
        } else {
            hSymbolBmp = LoadBitmap(hInst, szNoSymbol);
        }
    }

    if (!hSKBmp) {
        if (lpIMC->fdwConversion & IME_CMODE_SOFTKBD) {
            hSKBmp = LoadBitmap(hInst, szSoftKBD);
        } else {
            hSKBmp = LoadBitmap(hInst, szNoSoftKBD);
        }
    }

    ImmUnlockIMC(hIMC);
    ImmUnlockIMCC(lpIMC->hPrivate);

    hMemDC = CreateCompatibleDC(hDC);

    hOldBmp = SelectObject(hMemDC, hImeIconBmp);

    BitBlt(hDC, sImeG.rcImeIcon.left, sImeG.rcImeIcon.top,
           sImeG.rcImeIcon.right - sImeG.rcImeIcon.left,
           STATUS_DIM_Y,
           hMemDC, 0, 0, SRCCOPY);

    SelectObject(hMemDC, hShapeBmp);

    BitBlt(hDC, sImeG.rcShapeText.left, sImeG.rcShapeText.top,
           sImeG.rcShapeText.right - sImeG.rcShapeText.left,
           STATUS_DIM_Y,
           hMemDC, 0, 0, SRCCOPY);

    SelectObject(hMemDC, hSymbolBmp);

    BitBlt(hDC, sImeG.rcSymbol.left, sImeG.rcSymbol.top,
           sImeG.rcSymbol.right - sImeG.rcSymbol.left,
           STATUS_DIM_Y,
           hMemDC, 0, 0, SRCCOPY);

    SelectObject(hMemDC, hSKBmp);

    BitBlt(hDC, sImeG.rcSKText.left, sImeG.rcSKText.top,
           sImeG.xStatusWi - sImeG.rcSKText.left,
           STATUS_DIM_Y,
           hMemDC, 0, 0, SRCCOPY);

    SelectObject(hMemDC, hOldBmp);

    DeleteDC(hMemDC);

    DeleteObject(hImeIconBmp);
    DeleteObject(hSymbolBmp);
    DeleteObject(hShapeBmp);
    DeleteObject(hSKBmp);
    if (sImeG.fDiffSysCharSet) {
        DeleteObject(SelectObject(hDC, hOldFont));
    }

    return;
}

/**********************************************************************/
/* StatusWndProc()                                                    */
/**********************************************************************/
LRESULT CALLBACK StatusWndProc(
    HWND   hStatusWnd,
    UINT   uMsg,
    WPARAM wParam,
    LPARAM lParam)
{
    EnterLeaveDebug();
    switch (uMsg) {
    case WM_DESTROY:BHJDEBUG("case WM_DESTROY");
        DestroyStatusWindow(hStatusWnd);
        break;
    case WM_IME_NOTIFY:BHJDEBUG("case WM_IME_NOTIFY");
        // get work area for changing
        SystemParametersInfo(SPI_GETWORKAREA, 0, &sImeG.rcWorkArea, 0);

        if (wParam == IMN_SETSTATUSWINDOWPOS) {
            SetStatusWindowPos(hStatusWnd);
        }
        break;
    case WM_PAINT:BHJDEBUG("case WM_PAINT");
    {
        HDC         hDC;
        PAINTSTRUCT ps;

        hDC = BeginPaint(hStatusWnd, &ps);
        PaintStatusWindow(hDC, hStatusWnd);
        EndPaint(hStatusWnd, &ps);
    }
    break;
    case WM_MOUSEACTIVATE:BHJDEBUG("case WM_MOUSEACTIVATE");
        return (MA_NOACTIVATE);
    default:BHJDEBUG("default");
        return DefWindowProc(hStatusWnd, uMsg, wParam, lParam);
    }

    return (0L);
}


//Local Variables: ***
//coding: gb2312 ***
//End: ***
