/*++

  Copyright (c) 1990-1999 Microsoft Corporation, All Rights Reserved

  Module Name:

  candui.c

  ++*/

#include <windows.h>
#include <immdev.h>
#include <imedefs.h>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h"

/**********************************************************************/
/* GetCandWnd                                                         */
/* Return Value :                                                     */
/*      window handle of candidatte                                   */
/**********************************************************************/
HWND PASCAL GetCandWnd(
    HWND hUIWnd)                // UI window
{
    EnterLeaveDebug();
    HGLOBAL  hUIPrivate;
    LPUIPRIV lpUIPrivate;
    HWND     hCandWnd;

    hUIPrivate = (HGLOBAL)GetWindowLongPtr(hUIWnd, IMMGWLP_PRIVATE);
    if (!hUIPrivate) {          // can not darw candidate window
        return (HWND)NULL;
    }

    lpUIPrivate = (LPUIPRIV)GlobalLock(hUIPrivate);
    if (!lpUIPrivate) {         // can not draw candidate window
        return (HWND)NULL;
    }

    hCandWnd = lpUIPrivate->hCandWnd;

    GlobalUnlock(hUIPrivate);
    return (hCandWnd);
}

void PASCAL CalcCandPos(
    HIMC    hIMC,
    HWND    hUIWnd,
    LPPOINT lpptWnd)            // the composition window position
{
    EnterLeaveDebug();
    POINT          ptNew, ptSTWPos;
    RECT           rcWorkArea;


    rcWorkArea = ImeMonitorWorkAreaFromPoint(*lpptWnd);



    ptNew.x = lpptWnd->x + lpImeL->xCompWi + UI_MARGIN;
    if (ptNew.x + sImeG.xCandWi > rcWorkArea.right) {
        // exceed screen width
        ptNew.x = lpptWnd->x - sImeG.xCandWi - UI_MARGIN;
    }

    ptNew.y = lpptWnd->y + lpImeL->cyCompBorder - sImeG.cyCandBorder;
    if (ptNew.y + sImeG.yCandHi > rcWorkArea.bottom) {
        // exceed screen high
        ptNew.y = rcWorkArea.bottom - sImeG.yCandHi;
    }

    if(!sImeG.IC_Trace) {
        HWND hCompWnd;

        ImmGetStatusWindowPos(hIMC, (LPPOINT)&ptSTWPos);
        hCompWnd = GetCompWnd(hUIWnd);
        if (hCompWnd) {
            ptNew.x = ptSTWPos.x + sImeG.xStatusWi + lpImeL->xCompWi + 2 * UI_MARGIN;
            if((ptSTWPos.x + sImeG.xStatusWi + sImeG.xCandWi + lpImeL->xCompWi + 2 * UI_MARGIN)>
               rcWorkArea.right) {
                if (ptSTWPos.x >= (sImeG.xCandWi + lpImeL->xCompWi + 2 * UI_MARGIN)) { 
                    ptNew.x = ptSTWPos.x - lpImeL->xCompWi - sImeG.xCandWi - 2 * UI_MARGIN;
                } else {
                    ptNew.x = ptSTWPos.x + sImeG.xStatusWi + UI_MARGIN;
                }
            }


            ptNew.y = ptSTWPos.y + lpImeL->cyCompBorder - sImeG.cyCandBorder;
            if (ptNew.y + sImeG.yCandHi > rcWorkArea.bottom) {
                ptNew.y = rcWorkArea.bottom - sImeG.yCandHi;
            }
        } else {
            ptNew.x = ptSTWPos.x + sImeG.xStatusWi + UI_MARGIN;
            if(((ptSTWPos.x + sImeG.xStatusWi + sImeG.xCandWi + UI_MARGIN)>=
                rcWorkArea.right)
               && (ptSTWPos.x >= sImeG.xCandWi + UI_MARGIN)) { 
                ptNew.x = ptSTWPos.x - sImeG.xCandWi - UI_MARGIN;
            }

            ptNew.y = ptSTWPos.y + lpImeL->cyCompBorder - sImeG.cyCandBorder;
            if (ptNew.y + sImeG.yCandHi > rcWorkArea.bottom) {
                ptNew.y = rcWorkArea.bottom - sImeG.yCandHi;
            }
        }
    }
    
    lpptWnd->x = ptNew.x;
    lpptWnd->y = ptNew.y;

    return;
}

/**********************************************************************/
/* AdjustCandPos                                                      */
/**********************************************************************/
void AdjustCandPos(
    HIMC    hIMC,
    LPPOINT lpptWnd)            // the composition window position
{
    EnterLeaveDebug();
    LPINPUTCONTEXT lpIMC;
    LONG           ptFontHi;
    UINT           uEsc;
    RECT           rcWorkArea;


    rcWorkArea = ImeMonitorWorkAreaFromPoint(*lpptWnd);


    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {
        return;
    }

    if (lpIMC->lfFont.A.lfHeight > 0) {
        ptFontHi = lpIMC->lfFont.A.lfHeight;
    } else if (lpIMC->lfFont.A.lfWidth == 0) {
        ptFontHi = lpImeL->yCompHi;
    } else {
        ptFontHi = -lpIMC->lfFont.A.lfHeight;
    }

    if (ptFontHi > lpImeL->yCompHi * 8) {
        ptFontHi = lpImeL->yCompHi * 8;
    }

    if (ptFontHi < sImeG.yChiCharHi) {
        ptFontHi = sImeG.yChiCharHi;
    }

    // -450 to 450 index 0
    // 450 to 1350 index 1
    // 1350 to 2250 index 2
    // 2250 to 3150 index 3
    uEsc = (UINT)((lpIMC->lfFont.A.lfEscapement + 450) / 900 % 4);

    // find the location after IME do an adjustment
    ptFontHi = ptFontHi * ptInputEsc[uEsc].y;

    if(lpptWnd->y + ptFontHi + sImeG.yCandHi <= rcWorkArea.bottom) {
        lpptWnd->y += ptFontHi;
    } else {
        lpptWnd->y -= (ptFontHi + sImeG.yCandHi);
    }

    ImmUnlockIMC(hIMC);
    return;
}

/**********************************************************************/
/* AdjustCandRectBoundry                                              */
/**********************************************************************/
void PASCAL AdjustCandRectBoundry(
    LPINPUTCONTEXT lpIMC,
    LPPOINT        lpptCandWnd)            // the caret position
{
    EnterLeaveDebug();
    RECT  rcExclude, rcUIRect, rcInterSect;
    UINT  uEsc;
    RECT  rcWorkArea;



    {
        RECT rcCandWnd;

        *(LPPOINT)&rcCandWnd = *(LPPOINT)lpptCandWnd;

        rcCandWnd.right = rcCandWnd.left + sImeG.xCandWi;
        rcCandWnd.bottom = rcCandWnd.top + sImeG.yCandHi;

        rcWorkArea = ImeMonitorWorkAreaFromRect(&rcCandWnd);
    }



    // be a normal rectangle, not a negative rectangle
    if (lpIMC->cfCandForm[0].rcArea.left > lpIMC->cfCandForm[0].rcArea.right) {
        LONG tmp;

        tmp = lpIMC->cfCandForm[0].rcArea.left;
        lpIMC->cfCandForm[0].rcArea.left = lpIMC->cfCandForm[0].rcArea.right;
        lpIMC->cfCandForm[0].rcArea.right = tmp;
    }

    if (lpIMC->cfCandForm[0].rcArea.top > lpIMC->cfCandForm[0].rcArea.bottom) {
        LONG tmp;

        tmp = lpIMC->cfCandForm[0].rcArea.top;
        lpIMC->cfCandForm[0].rcArea.top = lpIMC->cfCandForm[0].rcArea.bottom;
        lpIMC->cfCandForm[0].rcArea.bottom = tmp;
    }

    // translate from client coordinate to screen coordinate
    rcExclude = lpIMC->cfCandForm[0].rcArea;

    rcExclude.left += lpptCandWnd->x - lpIMC->cfCandForm[0].ptCurrentPos.x;
    rcExclude.right += lpptCandWnd->x - lpIMC->cfCandForm[0].ptCurrentPos.x;

    rcExclude.top += lpptCandWnd->y - lpIMC->cfCandForm[0].ptCurrentPos.y;
    rcExclude.bottom += lpptCandWnd->y - lpIMC->cfCandForm[0].ptCurrentPos.y;

    // if original point is OK, we use it
    *(LPPOINT)&rcUIRect = *lpptCandWnd;

    if (rcUIRect.left < rcWorkArea.left) {
        rcUIRect.left = rcWorkArea.left;
    } else if (rcUIRect.left + sImeG.xCandWi > rcWorkArea.right) {
        rcUIRect.left = rcWorkArea.right - sImeG.xCandWi;
    } else {
    }

    if (rcUIRect.top < rcWorkArea.top) {
        rcUIRect.top = rcWorkArea.top;
    } else if (rcUIRect.top + sImeG.yCandHi > rcWorkArea.bottom) {
        rcUIRect.top = rcWorkArea.bottom - sImeG.yCandHi;
    } else {
    }

    rcUIRect.right = rcUIRect.left + sImeG.xCandWi;
    rcUIRect.bottom = rcUIRect.top + sImeG.yCandHi;

    if (!IntersectRect(&rcInterSect, &rcExclude, &rcUIRect)) {
        *lpptCandWnd = *(LPPOINT)&rcUIRect;
        return;
    }

    uEsc = (UINT)((lpIMC->lfFont.A.lfEscapement + 450) / 900 % 4);

    if (uEsc & 0x0001) {
        // 900 & 2700 we need change x coordinate
        if (ptInputEsc[uEsc].x > 0) {
            rcUIRect.left = rcExclude.right;
        } else {
            rcUIRect.left = rcExclude.left - sImeG.xCandWi;
        }
    } else {
        // 0 & 1800 we do not change x coordinate
        rcUIRect.left = lpptCandWnd->x;
    }

    if (rcUIRect.left < rcWorkArea.left) {
        rcUIRect.left = rcWorkArea.left;
    } else if (rcUIRect.left + sImeG.xCandWi > rcWorkArea.right) {
        rcUIRect.left = rcWorkArea.right - sImeG.xCandWi;
    } else {
    }

    if (uEsc & 0x0001) {
        // 900 & 2700 we do not change y coordinate
        rcUIRect.top = lpptCandWnd->y;
    } else {
        // 0 & 1800 we need change y coordinate
        if (ptInputEsc[uEsc].y > 0) {
            rcUIRect.top = rcExclude.bottom;
        } else {
            rcUIRect.top = rcExclude.top - sImeG.yCandHi;
        }
    }

    if (rcUIRect.top < rcWorkArea.top) {
        rcUIRect.top = rcWorkArea.top;
    } else if (rcUIRect.top + sImeG.yCandHi > rcWorkArea.bottom) {
        rcUIRect.top = rcWorkArea.bottom - sImeG.yCandHi;
    } else {
    }

    rcUIRect.right = rcUIRect.left + sImeG.xCandWi;
    rcUIRect.bottom = rcUIRect.top + sImeG.yCandHi;

    // the candidate window not overlapped with exclude rectangle
    // so we found a position
    if (!IntersectRect(&rcInterSect, &rcExclude, &rcUIRect)) {
        *lpptCandWnd = *(LPPOINT)&rcUIRect;
        return;
    }

    // adjust according to
    *(LPPOINT)&rcUIRect = *lpptCandWnd;

    if (uEsc & 0x0001) {
        // 900 & 2700 we prefer adjust x
        if (ptInputEsc[uEsc].x > 0) {
            rcUIRect.left = rcExclude.right;
        } else {
            rcUIRect.left = rcExclude.left - sImeG.xCandWi;
        }

        if (rcUIRect.left < rcWorkArea.left) {
        } else if (rcUIRect.left + sImeG.xCandWi > rcWorkArea.right) {
        } else {
            if (rcUIRect.top < rcWorkArea.top) {
                rcUIRect.top = rcWorkArea.top;
            } else if (rcUIRect.top + sImeG.yCandHi > rcWorkArea.bottom) {
                rcUIRect.top = rcWorkArea.bottom - sImeG.yCandHi;
            }

            *lpptCandWnd = *(LPPOINT)&rcUIRect;
            return;
        }

        // negative try
        if (ptInputEsc[uEsc].x > 0) {
            rcUIRect.left = rcExclude.left - sImeG.xCandWi;
        } else {
            rcUIRect.left = rcExclude.right;
        }

        if (rcUIRect.left < rcWorkArea.left) {
        } else if (rcUIRect.left + sImeG.xCandWi > rcWorkArea.right) {
        } else {
            if (rcUIRect.top < rcWorkArea.top) {
                rcUIRect.top = rcWorkArea.top;
            } else if (rcUIRect.top + sImeG.yCandHi > rcWorkArea.bottom) {
                rcUIRect.top = rcWorkArea.bottom - sImeG.yCandHi;
            }

            *lpptCandWnd = *(LPPOINT)&rcUIRect;
            return;
        }

        // negative try failure again, we use positive plus display adjust
        if (ptInputEsc[uEsc].x > 0) {
            rcUIRect.left = rcExclude.right;
        } else {
            rcUIRect.left = rcExclude.left - sImeG.xCandWi;
        }

        if (rcUIRect.left < rcWorkArea.left) {
            rcUIRect.left = rcWorkArea.left;
        } else if (rcUIRect.left + sImeG.xCandWi > rcWorkArea.right) {
            rcUIRect.left = rcWorkArea.right - sImeG.xCandWi;
        }

        if (rcUIRect.top < rcWorkArea.top) {
            rcUIRect.top = rcWorkArea.top;
        } else if (rcUIRect.top + sImeG.yCandHi > rcWorkArea.bottom) {
            rcUIRect.top = rcWorkArea.bottom - sImeG.yCandHi;
        }

        *lpptCandWnd = *(LPPOINT)&rcUIRect;
    } else {
        // 0 & 1800 we prefer adjust y
        if (ptInputEsc[uEsc].y > 0) {
            rcUIRect.top = rcExclude.bottom;
        } else {
            rcUIRect.top = rcExclude.top - sImeG.yCandHi;
        }

        if (rcUIRect.top < rcWorkArea.top) {
        } else if (rcUIRect.top + sImeG.yCandHi > rcWorkArea.bottom) {
        } else {
            if (rcUIRect.left < rcWorkArea.left) {
                rcUIRect.left = rcWorkArea.left;
            } else if (rcUIRect.left + sImeG.xCandWi > rcWorkArea.right) {
                rcUIRect.left = rcWorkArea.right - sImeG.xCandWi;
            }

            *lpptCandWnd = *(LPPOINT)&rcUIRect;
            return;
        }

        // negative try
        if (ptInputEsc[uEsc].y > 0) {
            rcUIRect.top = rcExclude.top - sImeG.yCandHi;
        } else {
            rcUIRect.top = rcExclude.bottom;
        }

        if (rcUIRect.top < rcWorkArea.top) {
        } else if (rcUIRect.top + sImeG.yCandHi > rcWorkArea.right) {
        } else {
            if (rcUIRect.left < rcWorkArea.left) {
                rcUIRect.left = rcWorkArea.left;
            } else if (rcUIRect.left + sImeG.xCandWi > rcWorkArea.right) {
                rcUIRect.left = rcWorkArea.right - sImeG.xCandWi;
            }

            *lpptCandWnd = *(LPPOINT)&rcUIRect;
            return;
        }

        // negative try failure again, we use positive plus display adjust
        if (ptInputEsc[uEsc].y > 0) {
            rcUIRect.top = rcExclude.bottom;
        } else {
            rcUIRect.top = rcExclude.top - sImeG.yCandHi;
        }

        if (rcUIRect.left < rcWorkArea.left) {
            rcUIRect.left = rcWorkArea.left;
        } else if (rcUIRect.left + sImeG.xCandWi > rcWorkArea.right) {
            rcUIRect.left = rcWorkArea.right - sImeG.xCandWi;
        }

        if (rcUIRect.top < rcWorkArea.top) {
            rcUIRect.top = rcWorkArea.top;
        } else if (rcUIRect.top + sImeG.yCandHi > rcWorkArea.bottom) {
            rcUIRect.top = rcWorkArea.bottom - sImeG.yCandHi;
        }

        *lpptCandWnd = *(LPPOINT)&rcUIRect;
    }

    return;
}

/**********************************************************************/
/* AdjustCandBoundry                                                  */
/**********************************************************************/
void PASCAL AdjustCandBoundry(
    LPPOINT lpptCandWnd)            // the position
{
    EnterLeaveDebug();

    RECT  rcWorkArea;


    {
        RECT rcCandWnd;

        *(LPPOINT)&rcCandWnd = *(LPPOINT)lpptCandWnd;

        rcCandWnd.right = rcCandWnd.left + sImeG.xCandWi;
        rcCandWnd.bottom = rcCandWnd.top + sImeG.yCandHi;

        rcWorkArea = ImeMonitorWorkAreaFromRect(&rcCandWnd);
    }

    if (lpptCandWnd->x < rcWorkArea.left) {
        lpptCandWnd->x = rcWorkArea.left;
    } else if (lpptCandWnd->x + sImeG.xCandWi > rcWorkArea.right) {
        lpptCandWnd->x = rcWorkArea.right - sImeG.xCandWi;
    }

    if (lpptCandWnd->y < rcWorkArea.top) {
        lpptCandWnd->y = rcWorkArea.top;
    } else if (lpptCandWnd->y + sImeG.yCandHi > rcWorkArea.bottom) {
        lpptCandWnd->y = rcWorkArea.bottom - sImeG.yCandHi;
    }

    return;
}

/**********************************************************************/
/* SetCandPosition()                                                  */
/**********************************************************************/
LRESULT PASCAL SetCandPosition(
    HWND hCandWnd)
{
    EnterLeaveDebug();
    HWND           hUIWnd;
    HIMC           hIMC;
    LPINPUTCONTEXT lpIMC;
    POINT          ptNew;

    hUIWnd = GetWindow(hCandWnd, GW_OWNER);

    hIMC = (HIMC)GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
    if (!hIMC) {
        return (1L);
    }

    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {
        return (1L);
    }

    ptNew = lpIMC->cfCandForm[0].ptCurrentPos;

    ClientToScreen((HWND)lpIMC->hWnd, &ptNew);

    if (lpIMC->cfCandForm[0].dwStyle & CFS_FORCE_POSITION) {
    } else if (lpIMC->cfCandForm[0].dwStyle == CFS_CANDIDATEPOS) {
        AdjustCandBoundry(&ptNew);
    } else if (lpIMC->cfCandForm[0].dwStyle == CFS_EXCLUDE) {
        if(!sImeG.IC_Trace) {
            CalcCandPos(hIMC, hUIWnd, &ptNew);
        }
        AdjustCandBoundry(&ptNew);
    }

    SetWindowPos(hCandWnd, NULL, ptNew.x, ptNew.y,
                 0, 0, SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOZORDER);

    ImmUnlockIMC(hIMC);

    return (0L);
}

/**********************************************************************/
/* ShowCand()                                                         */
/**********************************************************************/
void PASCAL ShowCand(           // Show the candidate window
    HWND    hUIWnd,
    int     nShowCandCmd)
{
    EnterLeaveDebug();
    HGLOBAL  hUIPrivate;
    LPUIPRIV lpUIPrivate;

    hUIPrivate = (HGLOBAL)GetWindowLongPtr(hUIWnd, IMMGWLP_PRIVATE);
    if (!hUIPrivate) {          // can not darw candidate window
        return;
    }

    lpUIPrivate = (LPUIPRIV)GlobalLock(hUIPrivate);
    if (!lpUIPrivate) {         // can not draw candidate window
        return;
    }

    if (lpUIPrivate->nShowCandCmd == nShowCandCmd) {
        goto SwCandNoChange;
    }

    if (nShowCandCmd == SW_HIDE) {
        lpUIPrivate->fdwSetContext &= ~(ISC_HIDE_CAND_WINDOW);
    }

    if (!lpUIPrivate->hCandWnd) {
        // not in show candidate window mode
    } else if (lpUIPrivate->nShowCandCmd == nShowCandCmd) {
    } else {
        if(nShowCandCmd == SW_HIDE) {
            uOpenCand = 0;
        } else {
            HIMC           hIMC;
            POINT          ptSTWPos;
            int            Comp_CandWndLen;
            RECT           rcWorkArea;

            uOpenCand = 1;

            // reset status window for LINE_UI(FIX_UI)
            hIMC = (HIMC)GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
            if (!hIMC) {
                goto ShowCand;
            }

            ImmGetStatusWindowPos(hIMC, (LPPOINT)&ptSTWPos);

            rcWorkArea = ImeMonitorWorkAreaFromPoint(ptSTWPos);

            Comp_CandWndLen = 0;
            if(uOpenCand) {
                Comp_CandWndLen += sImeG.xCandWi + UI_MARGIN;
                if(uStartComp) {
                    Comp_CandWndLen += lpImeL->xCompWi + UI_MARGIN;
                }
                if(ptSTWPos.x + sImeG.xStatusWi + Comp_CandWndLen > rcWorkArea.right) {
                    PostMessage(GetCompWnd(hUIWnd), 
                                WM_IME_NOTIFY, 
                                IMN_SETCOMPOSITIONWINDOW, 
                                0);
                }
            }
        }
        
    ShowCand:
        ShowWindow(lpUIPrivate->hCandWnd, nShowCandCmd);
        lpUIPrivate->nShowCandCmd = nShowCandCmd;
    }

SwCandNoChange:
    GlobalUnlock(hUIPrivate);
    return;
}

/**********************************************************************/
/* OpenCand                                                           */
/**********************************************************************/
void PASCAL OpenCand(
    HWND hUIWnd)
{
    EnterLeaveDebug();
    HGLOBAL        hUIPrivate;
    LPUIPRIV       lpUIPrivate;
    HIMC           hIMC;
    LPINPUTCONTEXT lpIMC;
    POINT          ptWnd;

    hUIPrivate = (HGLOBAL)GetWindowLongPtr(hUIWnd, IMMGWLP_PRIVATE);
    if (!hUIPrivate) {          // can not darw candidate window
        return;
    }

    lpUIPrivate = (LPUIPRIV)GlobalLock(hUIPrivate);
    if (!lpUIPrivate) {         // can not draw candidate window
        return;
    }

    lpUIPrivate->fdwSetContext |= ISC_SHOWUICANDIDATEWINDOW;

    hIMC = (HIMC)GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
    if (!hIMC) {
        goto OpenCandUnlockUIPriv;
    }

    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {
        goto OpenCandUnlockUIPriv;
    }

    if (lpIMC->cfCandForm[0].dwIndex == 0) {
        
        ptWnd = lpIMC->cfCandForm[0].ptCurrentPos;

        ClientToScreen(lpIMC->hWnd, &ptWnd);

        if (lpIMC->cfCandForm[0].dwStyle & CFS_FORCE_POSITION) {
        } else if (lpIMC->cfCandForm[0].dwStyle == CFS_EXCLUDE) {
            POINT ptCaret;

            AdjustCandBoundry(&ptWnd);
            if((!sImeG.IC_Trace) || (!GetCaretPos(&ptCaret))) {

                if(GetCompWnd(hUIWnd)) {
                    ptWnd.x = ptWnd.y = 0;
                    ClientToScreen(lpIMC->hWnd, &ptWnd);
                    ptWnd.x -= lpImeL->cxCompBorder + 1;
                    ptWnd.y -= lpImeL->cyCompBorder + 1;
                } else {
                    ptWnd.x = lpImeL->cxCompBorder + 1;
                    ptWnd.y = lpImeL->cyCompBorder + 1;
                }

                CalcCandPos(hIMC, hUIWnd, &ptWnd);

                lpIMC->cfCandForm[0].dwStyle |= CFS_CANDIDATEPOS;
                lpIMC->cfCandForm[0].ptCurrentPos = ptWnd;
                ScreenToClient(lpIMC->hWnd, &lpIMC->cfCandForm[0].ptCurrentPos);
            } else {
                AdjustCandPos(hIMC, &ptWnd);
            }
        } else if (lpIMC->cfCandForm[0].dwStyle == CFS_CANDIDATEPOS) {
            AdjustCandBoundry(&ptWnd);
        } else {
            if (lpUIPrivate->nShowCompCmd != SW_HIDE) {
                ptWnd.x = ptWnd.y = 0;
                ClientToScreen(lpUIPrivate->hCompWnd, &ptWnd);
            } else {
                ptWnd = lpIMC->cfCompForm.ptCurrentPos;
                ClientToScreen(lpIMC->hWnd, &ptWnd);
            }

            ptWnd.x -= lpImeL->cxCompBorder + 1;
            ptWnd.y -= lpImeL->cyCompBorder + 1;

            CalcCandPos(hIMC, hUIWnd, &ptWnd);

            lpIMC->cfCandForm[0].dwStyle |= CFS_CANDIDATEPOS;
            lpIMC->cfCandForm[0].ptCurrentPos = ptWnd;
            ScreenToClient(lpIMC->hWnd, &lpIMC->cfCandForm[0].ptCurrentPos);
        }
    } else {
        // make cand windows trace comp window !
        if (lpUIPrivate->nShowCompCmd != SW_HIDE) {
            ptWnd.x = ptWnd.y = 0;
            ClientToScreen(lpUIPrivate->hCompWnd, &ptWnd);
        } else {
            ptWnd = lpIMC->cfCompForm.ptCurrentPos;
            ClientToScreen(lpIMC->hWnd, &ptWnd);
        }

        ptWnd.x -= lpImeL->cxCompBorder + 1;
        ptWnd.y -= lpImeL->cyCompBorder + 1;

        CalcCandPos(hIMC, hUIWnd, &ptWnd);

        lpIMC->cfCandForm[0].dwStyle |= CFS_CANDIDATEPOS;
        lpIMC->cfCandForm[0].ptCurrentPos = ptWnd;
        ScreenToClient(lpIMC->hWnd, &lpIMC->cfCandForm[0].ptCurrentPos);
    }

    ImmUnlockIMC(hIMC);

    if (lpUIPrivate->hCandWnd) {
        SetWindowPos(lpUIPrivate->hCandWnd, 
                     NULL,
                     ptWnd.x, 
                     ptWnd.y,
                     0, 
                     0, 
                     SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOZORDER);
    } else {
        lpUIPrivate->hCandWnd = CreateWindowEx(
            WS_EX_WINDOWEDGE|WS_EX_DLGMODALFRAME,
            szCandClassName, 
            NULL, 
            WS_POPUP|WS_DISABLED,
            ptWnd.x,
            ptWnd.y,
            sImeG.xCandWi, 
            sImeG.yCandHi,
            hUIWnd, 
            (HMENU)NULL, 
            hInst, 
            NULL);

        SetWindowLong(lpUIPrivate->hCandWnd, UI_MOVE_OFFSET, WINDOW_NOT_DRAG);
        SetWindowLong(lpUIPrivate->hCandWnd, UI_MOVE_XY, 0L);
    }

    ShowCand(hUIWnd, SW_SHOWNOACTIVATE);

OpenCandUnlockUIPriv:
    GlobalUnlock(hUIPrivate);
    return;
}

/**********************************************************************/
/* CloseCand                                                          */
/**********************************************************************/
void PASCAL CloseCand(
    HWND hUIWnd)
{
    EnterLeaveDebug();
    uOpenCand = 0;
    ShowCand(hUIWnd, SW_HIDE);
    return;
}

/**********************************************************************/
/* DestroyCandWindow                                                  */
/**********************************************************************/
void PASCAL DestroyCandWindow(
    HWND hCandWnd)
{
    EnterLeaveDebug();
    HGLOBAL  hUIPrivate;
    LPUIPRIV lpUIPrivate;

    hUIPrivate = (HGLOBAL)GetWindowLongPtr(GetWindow(hCandWnd, GW_OWNER),
                                           IMMGWLP_PRIVATE);
    if (!hUIPrivate) {          // can not darw candidate window
        return;
    }

    lpUIPrivate = (LPUIPRIV)GlobalLock(hUIPrivate);
    if (!lpUIPrivate) {         // can not draw candidate window
        return;
    }

    lpUIPrivate->nShowCandCmd = SW_HIDE;

    lpUIPrivate->hCandWnd = (HWND)NULL;

    GlobalUnlock(hUIPrivate);
    return;
}


/**********************************************************************/
/* CandPageDownUP()                                                   */
/**********************************************************************/
void PASCAL CandPageDownUP(
    HWND hCandWnd,
    UINT uCandDownUp)
{
    EnterLeaveDebug();
    HIMC            hIMC;
    LPINPUTCONTEXT  lpIMC;
    LPPRIVCONTEXT   lpImcP;
    LPCANDIDATEINFO lpCandInfo;
    LPCANDIDATELIST lpCandList;
    HDC             hDC;
    HGDIOBJ         hCandHpBmp, hCandUpBmp, hCandDpBmp, hCandEpBmp;
    HGDIOBJ         hOldBmp;
    HDC             hMemDC;

    // change candlist
    hIMC = (HIMC)GetWindowLongPtr(GetWindow(hCandWnd, GW_OWNER), IMMGWLP_IMC);
    if (!hIMC) {
        return;
    }

    // get lpIMC
    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {
        return;
    }

    // get lpImcP
    lpImcP = (LPPRIVCONTEXT)ImmLockIMCC(lpIMC->hPrivate);
    if (!lpImcP) {
        return;
    }

    // get lpCandInfo
    lpCandInfo = (LPCANDIDATEINFO)ImmLockIMCC(lpIMC->hCandInfo);

    if (!lpCandInfo) {
        return;
    }
                                                 
    // get lpCandList and init dwCount & dwSelection
    lpCandList = (LPCANDIDATELIST)
        ((LPBYTE)lpCandInfo + lpCandInfo->dwOffset[0]);
    
    switch(uCandDownUp) {
    case uCandHome:BHJDEBUG("case uCandHome");
        ChooseCand(0x24, lpIMC, lpCandInfo, lpImcP);
        NotifyIME(hIMC, NI_CHANGECANDIDATELIST, 0, 0);
        break;
    case uCandUp:BHJDEBUG("case uCandUp");
        ChooseCand('-', lpIMC, lpCandInfo, lpImcP);
        NotifyIME(hIMC, NI_CHANGECANDIDATELIST, 0, 0);
        break;
    case uCandDown:BHJDEBUG("case uCandDown");
        ChooseCand('=', lpIMC, lpCandInfo, lpImcP);
        NotifyIME(hIMC, NI_CHANGECANDIDATELIST, 0, 0);
        break;
    case uCandEnd:BHJDEBUG("case uCandEnd");
        ChooseCand(0x23, lpIMC, lpCandInfo, lpImcP);
        NotifyIME(hIMC, NI_CHANGECANDIDATELIST, 0, 0);
        break;
    default:BHJDEBUG("default");
        break;
    }

    ImmUnlockIMCC(lpIMC->hPrivate);
    ImmUnlockIMCC(lpIMC->hCandInfo);
    ImmUnlockIMC(hIMC);

    // draw button down
    hDC = GetDC(hCandWnd);
    if ( hDC == NULL )
        return;

    hMemDC = CreateCompatibleDC(hDC);
    
    if ( hMemDC == NULL )
    {
        ReleaseDC(hCandWnd, hDC); 
        return;
    }

    hOldBmp = NULL;

    switch(uCandDownUp) {
    case uCandHome:BHJDEBUG("case uCandHome");

        hCandHpBmp = LoadBitmap(hInst, TEXT("CandHp"));
        if ( hCandHpBmp != NULL)
        {
            hOldBmp = SelectObject(hMemDC, hCandHpBmp);
            BitBlt(hDC, sImeG.rcCandBTH.left, sImeG.rcCandBTH.top,
                   sImeG.rcCandBTH.right - sImeG.rcCandBTH.left,
                   STATUS_DIM_Y,
                   hMemDC, 0, 0, SRCCOPY);
            DeleteObject(hCandHpBmp);
        }
            
        break;
    case uCandUp:BHJDEBUG("case uCandUp");

        hCandUpBmp = LoadBitmap(hInst, TEXT("CandUp"));
        if ( hCandUpBmp != NULL )
        {
            hOldBmp = SelectObject(hMemDC, hCandUpBmp);
            BitBlt(hDC, sImeG.rcCandBTU.left, sImeG.rcCandBTU.top,
                   sImeG.rcCandBTU.right - sImeG.rcCandBTU.left,
                   STATUS_DIM_Y,
                   hMemDC, 0, 0, SRCCOPY);
            DeleteObject(hCandUpBmp);
        }
        break;
    case uCandDown:BHJDEBUG("case uCandDown");
        hCandDpBmp = LoadBitmap(hInst, TEXT("CandDp"));
        if ( hCandDpBmp != NULL)
        {
            hOldBmp = SelectObject(hMemDC, hCandDpBmp);
            BitBlt(hDC, sImeG.rcCandBTD.left, sImeG.rcCandBTD.top,
                   sImeG.rcCandBTD.right - sImeG.rcCandBTD.left,
                   STATUS_DIM_Y,
                   hMemDC, 0, 0, SRCCOPY);
            DeleteObject(hCandDpBmp);
        }
        break;
    case uCandEnd:BHJDEBUG("case uCandEnd");
        hCandEpBmp = LoadBitmap(hInst, TEXT("CandEp"));
        if ( hCandEpBmp != NULL)
        {
            hOldBmp = SelectObject(hMemDC, hCandEpBmp);
            BitBlt(hDC, sImeG.rcCandBTE.left, sImeG.rcCandBTE.top,
                   sImeG.rcCandBTE.right - sImeG.rcCandBTE.left,
                   STATUS_DIM_Y,
                   hMemDC, 0, 0, SRCCOPY);
            DeleteObject(hCandEpBmp);
        }
        break;
    default:BHJDEBUG("default");
        break;
    }
       
    if ( hOldBmp != NULL) 
        SelectObject(hMemDC, hOldBmp);

    DeleteDC(hMemDC);
    ReleaseDC(hCandWnd, hDC);

    return;
}



/**********************************************************************/
/* UpdateCandWindow()                                                 */
/**********************************************************************/
void PASCAL PaintCandWindow(
    HWND hCandWnd,
    HDC  hDC)
{
    EnterLeaveDebug();
    HIMC            hIMC;
    LPINPUTCONTEXT  lpIMC;
    LPCANDIDATEINFO lpCandInfo;
    LPCANDIDATELIST lpCandList;
    LPPRIVCONTEXT   lpImcP;
    HGDIOBJ         hOldFont;
    DWORD           dwStart, dwEnd;
    TCHAR           szStrBuf[2 * MAXSTRLEN * sizeof(WCHAR) / sizeof(TCHAR) + 1];
    int             i;
    HGDIOBJ         hCandIconBmp, hCandInfBmp;
    HGDIOBJ         hOldBmp, hCandHBmp, hCandUBmp, hCandDBmp, hCandEBmp;
    HDC             hMemDC;

    hIMC = (HIMC)GetWindowLongPtr(GetWindow(hCandWnd, GW_OWNER), IMMGWLP_IMC);
    if (!hIMC) {
        return;
    }

    lpIMC = (LPINPUTCONTEXT)ImmLockIMC(hIMC);
    if (!lpIMC) {
        return;
    }

    if (!lpIMC->hCandInfo) {
        goto UpCandW2UnlockIMC;
    }

    lpCandInfo = (LPCANDIDATEINFO)ImmLockIMCC(lpIMC->hCandInfo);
    if (!lpCandInfo) {
        goto UpCandW2UnlockIMC;
    }

    if (!lpIMC->hPrivate) {
        goto UpCandW2UnlockCandInfo;
    }

    lpImcP = (LPPRIVCONTEXT)ImmLockIMCC(lpIMC->hPrivate);
    if (!lpImcP) {
        goto UpCandW2UnlockCandInfo;
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

    lpCandList = (LPCANDIDATELIST)((LPBYTE)lpCandInfo +
                                   lpCandInfo->dwOffset[0]);

    dwStart = lpCandList->dwSelection;
    dwEnd = dwStart + lpCandList->dwPageSize;

    if (dwEnd > lpCandList->dwCount) {
        dwEnd = lpCandList->dwCount;
    }

    SetTextColor(hDC, RGB(0x00, 0x00, 0x00));
    SetBkColor(hDC, RGB(0xff, 0xff, 0xff));

    if (sImeG.IC_Trace) {
        ExtTextOut(hDC, sImeG.rcCandText.left, sImeG.rcCandText.top,
                   ETO_OPAQUE, &sImeG.rcCandText, NULL, 0, NULL);
        szStrBuf[0] = TEXT('1');
        szStrBuf[1] = TEXT(':');

        for (i = 0; dwStart < dwEnd; dwStart++, i++) {
            int  iLen;

            iLen = 0;

            szStrBuf[0] = szDigit[i + CAND_START];


            if(sImeL.dwRegImeIndex == INDEX_UNICODE){
                WORD  wCode;
                wCode = *(LPUNAWORD)((LPBYTE)lpCandList + lpCandList->dwOffset[dwStart]);


                szStrBuf[2]= wCode;
                szStrBuf[3]=TEXT('\0');

                iLen = 2/sizeof(TCHAR);
            }



            ExtTextOut(hDC, sImeG.rcCandText.left,
                       sImeG.rcCandText.top + i * sImeG.yChiCharHi,
                       (UINT)0, NULL,
                       szStrBuf,
                       iLen + 2, NULL);
    
            // QW/GB info
            {

                int   iMyLen;
                WORD  wCode, wGB;
                TCHAR  AbSeq[5];
                TCHAR  GbSeq[5];
                TCHAR szMyStrBuf[12 * sizeof(WCHAR) / sizeof(TCHAR)];
                RECT  GBARInfo;




                wCode = *(LPUNAWORD)((LPBYTE)lpCandList + lpCandList->dwOffset[dwStart]);
                AbSeq[0] = wCode;
                AbSeq[1] = TEXT('\0');
                //  change the CP_ACP to 936, so that it can work under Multilingul Env.
                WideCharToMultiByte(NATIVE_ANSI_CP, WC_COMPOSITECHECK, AbSeq, 1, (LPSTR)GbSeq, sizeof(GbSeq), NULL, NULL);
                          
                wGB = HIBYTE(GbSeq[0]) | (LOBYTE(GbSeq[0]) << 8);

                wsprintf (GbSeq,TEXT("%04lx"),wGB);    // get GB string
                wGB -= 0xa0a0;
                wsprintf (AbSeq,TEXT("%02d%02d"),HIBYTE(wGB),LOBYTE(wGB));

//             if (lpImcP->fdwGB & IME_SELECT_GB) {

                switch(sImeL.dwRegImeIndex){
                case INDEX_UNICODE:BHJDEBUG("case INDEX_UNICODE");        //adjust code display info
                    lstrcpy (szMyStrBuf,TEXT("("));
                    lstrcat (szMyStrBuf,GbSeq);
                    lstrcat (szMyStrBuf,TEXT(", "));
                    wsprintf (AbSeq,TEXT("%04lx"),wCode);
                    lstrcat (szMyStrBuf, AbSeq);
                    lstrcat (szMyStrBuf,TEXT(")"));
                    iMyLen = lstrlen(szMyStrBuf);
                    break;
                }

        
                GBARInfo.top = sImeG.rcCandText.top + i * sImeG.yChiCharHi;
                GBARInfo.left = sImeG.rcCandText.left;
                GBARInfo.right = sImeG.rcCandText.right;
                GBARInfo.bottom = sImeG.rcCandText.bottom;
                DrawText(hDC, szMyStrBuf, lstrlen(szMyStrBuf),
                         &GBARInfo, DT_RIGHT | DT_SINGLELINE);
            }
        }

    } else {
        int  nX;

        ExtTextOut(hDC, sImeG.rcCandText.left, sImeG.rcCandText.top + 1,
                   ETO_OPAQUE, &sImeG.rcCandText, NULL, 0, NULL);
        nX = 0;
        for (i = 0; dwStart < dwEnd; dwStart++, i++) {
            int  iLen;
            int j, k;
            TCHAR AnsiStr[MAXCODE+1];
            SIZE StrSize;

            // display numbers
            AnsiStr[0] = szDigit[i + CAND_START];
            AnsiStr[1] = TEXT(':');
            AnsiStr[2] = 0;
            ExtTextOut(hDC, sImeG.rcCandText.left + nX,
                       sImeG.rcCandText.top + 1,
                       ETO_CLIPPED, &sImeG.rcCandText,
                       AnsiStr,
                       lstrlen(AnsiStr), NULL);
            if(!GetTextExtentPoint(hDC, (LPCTSTR)AnsiStr, lstrlen(AnsiStr), &StrSize))
                memset(&StrSize, 0, sizeof(SIZE));
            nX += StrSize.cx;

            // display chinese word and code
            iLen = lstrlen((LPTSTR)((LPBYTE)lpCandList +
                                    lpCandList->dwOffset[dwStart]));

            CopyMemory(szStrBuf,
                       ((LPBYTE)lpCandList + lpCandList->dwOffset[dwStart]),
                       iLen*sizeof(TCHAR));

            for(j=0; j<iLen; j++) {

                if(szStrBuf[j] > 0x100) {

                    j++;
                    continue;
                }
                break;
            }
            
            k = j-1;
            for(j=0; j<iLen - k; j++) {
                AnsiStr[j] = szStrBuf[j+k];
            }
            AnsiStr[j] = 0;
            szStrBuf[k] = 0;


            ExtTextOut(hDC, sImeG.rcCandText.left + nX,
                       sImeG.rcCandText.top + 1,
                       ETO_CLIPPED, &sImeG.rcCandText,
                       szStrBuf,
                       lstrlen(szStrBuf), NULL);

            if(!GetTextExtentPoint(hDC, (LPCTSTR)szStrBuf, lstrlen(szStrBuf), &StrSize))
                memset(&StrSize, 0, sizeof(SIZE));
            nX += StrSize.cx;

            ExtTextOut(hDC, sImeG.rcCandText.left + nX,
                       sImeG.rcCandText.top + 1,
                       ETO_CLIPPED, &sImeG.rcCandText,
                       AnsiStr,
                       lstrlen(AnsiStr), NULL);

            if(!GetTextExtentPoint(hDC, (LPCTSTR)AnsiStr, lstrlen(AnsiStr), &StrSize))
                memset(&StrSize, 0, sizeof(SIZE));
            nX += StrSize.cx;

        }
    }
    
    // load all bitmap
    if (sImeG.IC_Trace) {
        hCandInfBmp = LoadBitmap(hInst, TEXT("Candinf"));
    } else {
        hCandInfBmp = NULL;
    }


    hMemDC = CreateCompatibleDC(hDC);

    if ( hMemDC != NULL )
    {

        hCandIconBmp = LoadBitmap(hInst, TEXT("CandSel"));

        if ( hCandIconBmp )
        {

            hOldBmp = SelectObject(hMemDC, hCandIconBmp);

            BitBlt(hDC, sImeG.rcCandIcon.left, sImeG.rcCandIcon.top,
                   sImeG.rcCandIcon.right - sImeG.rcCandIcon.left,
                   STATUS_DIM_Y,
                   hMemDC, 0, 0, SRCCOPY);

            SelectObject(hMemDC, hOldBmp);

            DeleteObject(hCandIconBmp);
        }

        if(hCandInfBmp) {
            
            hOldBmp = SelectObject(hMemDC, hCandInfBmp);

            BitBlt(hDC, sImeG.rcCandInf.left, sImeG.rcCandInf.top,
                   sImeG.rcCandInf.right - sImeG.rcCandInf.left,
                   STATUS_DIM_Y,
                   hMemDC, 0, 0, SRCCOPY);

            SelectObject(hMemDC, hOldBmp);
        }

        hCandHBmp = LoadBitmap(hInst, TEXT("CandH"));

        if ( hCandHBmp )
        {
            hOldBmp = SelectObject(hMemDC, hCandHBmp);

            BitBlt(hDC, sImeG.rcCandBTH.left, sImeG.rcCandBTH.top,
                   sImeG.rcCandBTH.right - sImeG.rcCandBTH.left,
                   STATUS_DIM_Y,
                   hMemDC, 0, 0, SRCCOPY);

            SelectObject(hMemDC, hOldBmp);

            DeleteObject(hCandHBmp);
        }


        hCandUBmp = LoadBitmap(hInst, TEXT("CandU"));

        if ( hCandUBmp )
        {
            hOldBmp = SelectObject(hMemDC, hCandUBmp);

            BitBlt(hDC, sImeG.rcCandBTU.left, sImeG.rcCandBTU.top,
                   sImeG.rcCandBTU.right - sImeG.rcCandBTU.left,
                   STATUS_DIM_Y,
                   hMemDC, 0, 0, SRCCOPY);

            SelectObject(hMemDC, hOldBmp);

            DeleteObject(hCandUBmp);
        }


        hCandDBmp = LoadBitmap(hInst, TEXT("CandD"));
        
        if ( hCandDBmp )
        {

            hOldBmp = SelectObject(hMemDC, hCandDBmp);

            BitBlt(hDC, sImeG.rcCandBTD.left, sImeG.rcCandBTD.top,
                   sImeG.rcCandBTD.right - sImeG.rcCandBTD.left,
                   STATUS_DIM_Y,
                   hMemDC, 0, 0, SRCCOPY);

            SelectObject(hMemDC, hOldBmp);

            DeleteObject(hCandDBmp);
        }

        hCandEBmp = LoadBitmap(hInst, TEXT("CandE"));
        
        if ( hCandEBmp )
        {
            hOldBmp = SelectObject(hMemDC, hCandEBmp);

            BitBlt(hDC, sImeG.rcCandBTE.left, sImeG.rcCandBTE.top,
                   sImeG.rcCandBTE.right - sImeG.rcCandBTE.left,
                   STATUS_DIM_Y,
                   hMemDC, 0, 0, SRCCOPY);

            SelectObject(hMemDC, hOldBmp);
            
            DeleteObject(hCandEBmp);
        }
        
        DeleteDC(hMemDC);

    }

    if ( hCandInfBmp )
        DeleteObject(hCandInfBmp);

    if (sImeG.fDiffSysCharSet) {
        DeleteObject(SelectObject(hDC, hOldFont));
    }

    ImmUnlockIMCC(lpIMC->hPrivate);
UpCandW2UnlockCandInfo:
    ImmUnlockIMCC(lpIMC->hCandInfo);
UpCandW2UnlockIMC:
    ImmUnlockIMC(hIMC);
    return;
}

/**********************************************************************/
/* CandWndProc()                                                      */
/**********************************************************************/
LRESULT CALLBACK CandWndProc(
    HWND   hCandWnd,
    UINT   uMsg,
    WPARAM wParam,
    LPARAM lParam)
{
    EnterLeaveDebug();
    switch (uMsg) {
    case WM_DESTROY:BHJDEBUG("case WM_DESTROY");
        DestroyCandWindow(hCandWnd);
        break;

    case WM_IME_NOTIFY:BHJDEBUG("case WM_IME_NOTIFY");
        if (wParam == IMN_SETCANDIDATEPOS) {
            return SetCandPosition(hCandWnd);
        }
        break;
    case WM_PAINT:BHJDEBUG("case WM_PAINT");
    {
        HDC         hDC;
        PAINTSTRUCT ps;

        hDC = BeginPaint(hCandWnd, &ps);
        PaintCandWindow(hCandWnd, hDC);
        EndPaint(hCandWnd, &ps);
    }
    break;
    case WM_MOUSEACTIVATE:BHJDEBUG("case WM_MOUSEACTIVATE");
        return (MA_NOACTIVATE);
    default:BHJDEBUG("default");
        return DefWindowProc(hCandWnd, uMsg, wParam, lParam);
    }

    return (0L);
}

//Local Variables: ***
//coding: gb2312 ***
//End: ***
