#include <afxwin.h>
class CMainFrame : public CFrameWnd
{
public:
	CMainFrame();
protected:
	DECLARE_MESSAGE_MAP()
};

CMainFrame::CMainFrame()
{

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
     }
     
     hwnd = CreateWindow (szAppName,                  // window class name
                          TEXT ("The Hello Program"), // window caption
						  WS_POPUP,
                          8,              // initial x position
                          8,              // initial y position
                          400,              // initial x size
                          400,              // initial y size
                          NULL,                       // parent window handle
                          NULL,                       // window menu handle
                          AfxGetInstanceHandle(),                  // program instance handle
                          NULL) ;                     // creation parameters

	 SubclassWindow(hwnd);

	// Create(NULL, "Windows Application", WS_POPUP, 
	//        CRect(120, 100, 700, 480), NULL);
}

BEGIN_MESSAGE_MAP(CMainFrame, CFrameWnd)
END_MESSAGE_MAP()


class CExerciseApp: public CWinApp
{
public:
	BOOL InitInstance();
};
BOOL CExerciseApp::InitInstance()
{
	m_pMainWnd = new CMainFrame ;
	m_pMainWnd->ModifyStyle(WS_CAPTION|WS_OVERLAPPED|WS_THICKFRAME, WS_CLIPCHILDREN); //Remove the titilebar from the frame using the ModifyStyle
	m_pMainWnd->ModifyStyleEx(WS_EX_CLIENTEDGE,0);			//Remove the border
	m_pMainWnd->ShowWindow(SW_SHOW);
	m_pMainWnd->UpdateWindow();
	return TRUE;
}
CExerciseApp theApp;
