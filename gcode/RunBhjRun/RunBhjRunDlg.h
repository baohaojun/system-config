// RunBhjRunDlg.h : header file
//

#if !defined(AFX_RUNBHJRUNDLG_H__CFE24788_6510_417E_BD42_731DD245BC4E__INCLUDED_)
#define AFX_RUNBHJRUNDLG_H__CFE24788_6510_417E_BD42_731DD245BC4E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CRunBhjRunDlg dialog

class CRunBhjRunDlg : public CDialog
{
// Construction
public:
	CRunBhjRunDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CRunBhjRunDlg)
	enum { IDD = IDD_RUNBHJRUN_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CRunBhjRunDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(CRunBhjRunDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_RUNBHJRUNDLG_H__CFE24788_6510_417E_BD42_731DD245BC4E__INCLUDED_)
