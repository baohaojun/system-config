#if !defined(AFX_EKBEDIT_H__07E08AF6_1197_41EE_B6F1_30723513D39F__INCLUDED_)
#define AFX_EKBEDIT_H__07E08AF6_1197_41EE_B6F1_30723513D39F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// EkbEdit.h : header file
//


/////////////////////////////////////////////////////////////////////////////
// CEkbEdit window
typedef enum {
	eNone = 0,
	eCtrl=1,
	eAlt=2,
	eShift=4,
	eCtrlAlt = eCtrl|eAlt,
	eCtrlAltShift= eCtrl|eAlt|eShift,
	eAltShift = eAlt|eShift,
	eCtrlShift = eCtrl|eShift,
} specKeyState_t;

class CEkbHistWnd;

#include <list>
using std::list;

#include "bhjlib.h"
class CHListBox;

using namespace bhj;

CRect GetClientRect(CWnd* wnd);
CRect GetWindowRect(CWnd* wnd);
HWND getTopParentHwnd(CWnd* wnd);
class CEkbEdit : public CEdit
{
// Construction
public:
	CEkbEdit();

// Attributes
public:

// Operations
public:
	void weVeMoved();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEkbEdit)
	public:
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CEkbEdit();

	int setHistFile(const CString& strFileName);
	void createListBox();
	void setListBox(CHListBox& listBox);


	
private:
	CRect getSelectedRect();
	specKeyState_t getSpecKeyState();
	void selectNextItem();
	void selectPrevItem(int prev=1);
	void getTextFromSelectedItem();

	void endOfLine();
	void beginOfLine();
	void killEndOfLine();
	void killBeginOfLine();
	void forwardChar();
	void backwardChar();
	void deleteChar();
	int GetLength();
	void backwardWord();
	void forwardWord();
	void backwardKillWord();
	void forwardKillWord();
	void escapeEdit();
	CString getText();
	void fillListBox(const CString& text);
	CHListBox* m_listBox;
	CEkbHistWnd* m_simpleWnd;
	UINT m_id;
	typedef list<string> lstring_t;
	lstring_t m_histList;

	CString m_strHistFile;
	void saveHist();
	cstring getSelectedText();
	void SetWindowText(const cstring& str);
	void SetWindowText(const CString& str);
	// Generated message map functions
protected:
	//{{AFX_MSG(CEkbEdit)
	afx_msg BOOL OnChange();
	afx_msg void OnKillFocus(CWnd* pNewWnd);
	afx_msg void OnSetFocus(CWnd* pOldWnd);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// CEkbHistWnd window

class CEkbHistWnd : public CWnd
{
// Construction
public:
	CEkbHistWnd(CEdit* master);
	CHListBox* m_listBox;
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
	//{{AFX_VIRTUAL(CEkbHistWnd)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CEkbHistWnd();

	// Generated message map functions
protected:
	//{{AFX_MSG(CEkbHistWnd)
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	afx_msg void OnPaint();
	afx_msg void OnSize(UINT nType, int cx, int cy);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
// CBalloon window

#define getBalloon(owner) (CBalloon::getInstance(owner))
class CBalloon : public CWnd
{
// Construction
public:
	static CBalloon* getInstance(CWnd* owner);
private:
	CBalloon(CWnd* owner);

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CBalloon)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CBalloon();

private:
	cstring m_text;

public:
	void showBalloon(CRect rect, const cstring& text);
	
	// Generated message map functions
protected:
	//{{AFX_MSG(CBalloon)
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	afx_msg void OnPaint();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// CHListBox window

class CHListBox : public CListBox
{
// Construction
public:
	CHListBox();

// Attributes
public:

// Operations
public:
	int SetCurSel(int nSelect);
	cstring getSelectedText();
    int AddString(LPCTSTR s);
    int InsertString(int i, LPCTSTR s);
    void ResetContent();
    int DeleteString(int i);
	CRect GetItemRect(int idx);
	CRect getSelectedRect();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CHListBox)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CHListBox();

	// Generated message map functions
protected:
        void updateWidth(LPCTSTR s);
	int width;
	//{{AFX_MSG(CHListBox)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EKBEDIT_H__07E08AF6_1197_41EE_B6F1_30723513D39F__INCLUDED_)
