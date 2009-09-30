// RunBhjRunDlg.cpp : implementation file
//

#include "stdafx.h"
#include "RunBhjRun.h"
#include "EkbEdit.h"
#include "RunBhjRunDlg.h"
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CRunBhjRunDlg dialog

CRunBhjRunDlg::CRunBhjRunDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CRunBhjRunDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CRunBhjRunDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CRunBhjRunDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CRunBhjRunDlg)
	DDX_Control(pDX, IDC_CmdEdit, m_CmdEdit);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CRunBhjRunDlg, CDialog)
	//{{AFX_MSG_MAP(CRunBhjRunDlg)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_WM_MOVE()
	ON_WM_WINDOWPOSCHANGED()
	//}}AFX_MSG_MAP
	ON_BN_CLICKED(IDC_BROWSE, CRunBhjRunDlg::OnBnClickedBrowse)
	ON_BN_CLICKED(IDOK, &CRunBhjRunDlg::OnBnClickedOk)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CRunBhjRunDlg message handlers

BOOL CRunBhjRunDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	m_CmdEdit.createListBox();
	m_CmdEdit.setHistFile("RunBhjRun.txt");
	
	// TODO: Add extra initialization here
	
	return TRUE;  // return TRUE  unless you set the focus to a control
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CRunBhjRunDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CRunBhjRunDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

void CRunBhjRunDlg::OnBnClickedBrowse()
{
 
}

void CRunBhjRunDlg::OnOK() 
{
	// TODO: Add extra validation here
	
	//CDialog::OnOK();
}

void CRunBhjRunDlg::OnMove(int x, int y) 
{
	CDialog::OnMove(x, y);
	
	m_CmdEdit.weVeMoved();
	
}

void CRunBhjRunDlg::OnWindowPosChanged(WINDOWPOS FAR* lpwndpos) 
{
	CDialog::OnWindowPosChanged(lpwndpos);
	BHJDEBUG(" OnWindowPosChanged");
	
	
	
}

void CRunBhjRunDlg::OnBnClickedOk()
{
	// TODO: Add your control notification handler code here
	OnOK();
}
