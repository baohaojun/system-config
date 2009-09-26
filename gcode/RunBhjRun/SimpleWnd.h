#if !defined(AFX_SIMPLEWND_H__AE32AF9D_B792_49DE_AE8C_1B8FFD1BF500__INCLUDED_)
#define AFX_SIMPLEWND_H__AE32AF9D_B792_49DE_AE8C_1B8FFD1BF500__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// SimpleWnd.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CSimpleWnd window

class CSimpleWnd : public CWnd
{
// Construction
public:
	CSimpleWnd(CEdit* master);
	CListBox* m_listBox;
private:
	CEdit* m_master;


// Attributes
public:

// Operations
public:
	void hide();
	void show();
	void weVeMoved();
private:
	void calcWindowRect(CRect& rect);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CSimpleWnd)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CSimpleWnd();

	// Generated message map functions
protected:
	//{{AFX_MSG(CSimpleWnd)
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	afx_msg void OnPaint();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SIMPLEWND_H__AE32AF9D_B792_49DE_AE8C_1B8FFD1BF500__INCLUDED_)
