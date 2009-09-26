// SimpleWnd.cpp : implementation file
//

#include "stdafx.h"
#include "SimpleWnd.h"
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSimpleWnd

CSimpleWnd::CSimpleWnd(CEdit* master)
{
	m_master = master;
	CRect rect;
	::GetWindowRect(m_master->GetSafeHwnd(), &rect);
//	m_master->ClientToScreen(&rect);
	
	int x = rect.left;
	int y = rect.bottom+2;
	BHJDEBUG(" x is %d, y is %d", x, y);
	int w = rect.Width();
	int h = rect.Height()*10;
     static TCHAR szAppName[] = TEXT ("HelloWin") ;
     HWND         hwnd ;
     WNDCLASS     wndclass ;

     wndclass.style         = CS_HREDRAW | CS_VREDRAW ;
     wndclass.lpfnWndProc   = ::DefWindowProc ;
     wndclass.cbClsExtra    = 0 ;
     wndclass.cbWndExtra    = 0 ;
     wndclass.hInstance     = AfxGetInstanceHandle() ;
     wndclass.hIcon         = LoadIcon (NULL, IDI_APPLICATION) ;
     wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
     wndclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH) ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;

     if (!RegisterClass (&wndclass))
     {
		 MessageBox ("This program requires Windows NT!");
		 return;
     }
     
     hwnd = CreateWindow (szAppName,                  // window class name
                          TEXT ("The Hello Program"), // window caption
						  WS_POPUP|WS_DISABLED|WS_CLIPCHILDREN,
                          x,              // initial x position
                          y,              // initial y position
                          w,              // initial x size
                          h,              // initial y size
                          NULL,                       // parent window handle
                          NULL,                       // window menu handle
                          AfxGetInstanceHandle(),                  // program instance handle
                          NULL) ;                     // creation parameters

	 SubclassWindow(hwnd);
	 ModifyStyleEx(0, WS_EX_TOOLWINDOW);
	 m_listBox = new CListBox();
	 GetClientRect(&rect);
	 rect.DeflateRect(1, 1);
	 m_listBox->Create(WS_VSCROLL|LBS_NOTIFY|LBS_SORT|LBS_MULTIPLESEL|LBS_NOINTEGRALHEIGHT, rect, this, 0);
	 m_listBox->ShowWindow(SW_SHOWNA);
	 m_listBox->UpdateWindow();

}

CSimpleWnd::~CSimpleWnd()
{
}


BEGIN_MESSAGE_MAP(CSimpleWnd, CWnd)
	//{{AFX_MSG_MAP(CSimpleWnd)
	ON_WM_SHOWWINDOW()
	ON_WM_PAINT()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CSimpleWnd message handlers

void CSimpleWnd::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CWnd::OnShowWindow(bShow, nStatus);
	CRect rect;
	calcWindowRect(rect);
	SetWindowPos(&wndTopMost, rect.left, rect.top, rect.Width(), rect.Height(), SWP_NOACTIVATE);
}

void CSimpleWnd::calcWindowRect(CRect& rect)
{
	m_master->GetWindowRect(&rect);
	BHJDEBUG(" top is %d, left is %d, height is %d, width is %d", rect.top, rect.left, rect.Height(), rect.Width());
	CRect tmpRect = rect;
	int top = rect.bottom + 2;
	int left = rect.left;

	rect.OffsetRect(0, top-tmpRect.top);
	BHJDEBUG(" again top is %d, left is %d, height is %d, width is %d", rect.top, rect.left, rect.Height(), rect.Width());	
	rect.bottom += rect.Height()*9;
	BHJDEBUG(" again top is %d, left is %d, height is %d, width is %d", rect.top, rect.left, rect.Height(), rect.Width());	

	RECT waRect;
	SystemParametersInfo(SPI_GETWORKAREA, 0, &waRect, 0);


	if (rect.bottom > waRect.bottom) {
		BHJDEBUG(" hello world");
		int bottom = tmpRect.top - 2;
		int height = rect.Height();
		rect.bottom = bottom;
		rect.top =bottom - height;
	}
}

void CSimpleWnd::hide()
{
	ShowWindow(SW_HIDE);
}

void CSimpleWnd::show()
{
	ShowWindow(SW_SHOWNA);
}

void CSimpleWnd::weVeMoved()
{
	CRect rect;
	calcWindowRect(rect);
	SetWindowPos(&wndTopMost, rect.left, rect.top, rect.Width(), rect.Height(), SWP_NOACTIVATE);
}

void CSimpleWnd::OnPaint() 
{
	CPaintDC dc(this); // device context for painting
	CRect rect;
	GetClientRect(&rect);
	dc.FillSolidRect(&rect, RGB(0, 0, 0));
	
	
}
