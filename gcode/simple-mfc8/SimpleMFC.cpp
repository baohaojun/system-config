#include <afxwin.h>
class CMainFrame : public CFrameWnd
{
public:
	CMainFrame();
protected:
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	DECLARE_MESSAGE_MAP()
};

CMainFrame::CMainFrame()
{
	Create(NULL, "Windows Application", WS_OVERLAPPEDWINDOW,
	       CRect(120, 100, 700, 480), NULL);
}

BEGIN_MESSAGE_MAP(CMainFrame, CFrameWnd)
ON_WM_CREATE()
END_MESSAGE_MAP()

int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if( CFrameWnd::OnCreate(lpCreateStruct) == 0)
	{
		MessageBox("The window has been created!!!");
		return 0;
	}
	return -1;
}

class CExerciseApp: public CWinApp
{
public:
	BOOL InitInstance();
};
BOOL CExerciseApp::InitInstance()
{
	m_pMainWnd = new CMainFrame ;
	m_pMainWnd->ShowWindow(SW_SHOW);
	m_pMainWnd->UpdateWindow();
	return TRUE;
}
CExerciseApp theApp;
