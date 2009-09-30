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
LOGFONT getLogFont(CFont*);
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
	void set_mark_command();
	void keyboard_quit();
	void move_to(int pos);
	void debug_caret();
	void exchange_point_and_mark();
	void kill_region();
	void delete_range(int start, int end);
	void yank();
	void kill_ring_save();
	cstring getText();
	void fillListBox(const CString& text);
	lstring_t getMatchingStrings(const cstring& text);
	int getPoint(); //emacs term, point:-)
	void setsel(int to);
	void SetSel(long start, long end);
	void shift_move(long start, long end);
	CPoint PosFromChar(int point);
	int CharFromPos(CPoint pnt);
	LONG getTextWidth(cstring str);
	CSize getTextSize(cstring text);
	LONG getTextHeight(cstring str);
	cstring getSubText(int start, int end);

private: //member vars
	CHListBox* m_listBox;
	CEkbHistWnd* m_simpleWnd;
	UINT m_id;
	lstring_t m_histList;
	int m_mark; //emacs term, mark;
	

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
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnWindowPosChanging(WINDOWPOS FAR* lpwndpos);
	afx_msg HBRUSH CtlColor(CDC* pDC, UINT nCtlColor);
	afx_msg void OnHscroll();
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
	enum {m_border = 5};
	CFont m_font;

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
	CSize getTextSize(cstring str);
	LONG getTextWidth(cstring str);
	

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
	afx_msg void OnSelchange();
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EKBEDIT_H__07E08AF6_1197_41EE_B6F1_30723513D39F__INCLUDED_)
