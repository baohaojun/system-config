#include <afxwin.h>
class CSimpleFrame : public CFrameWnd
{
public:
	CSimpleFrame()
		{
// Create the window's frame
			Create(NULL, "Windows Application");
		}
};
struct CSimpleApp : public CWinApp
{
	BOOL InitInstance()
		{
// Use a pointer to the window's frame for the application
// to use the window
			CSimpleFrame *Tester = new CSimpleFrame ();
			m_pMainWnd = Tester;
// Show the window
			m_pMainWnd->ShowWindow(SW_SHOW);
			m_pMainWnd->UpdateWindow();
			return TRUE;
		}
};
CSimpleApp theApp;
