#if !defined(AFX_EMACSEDIT_H__5046851A_1A40_4795_897E_16254776806A__INCLUDED_)
#define AFX_EMACSEDIT_H__5046851A_1A40_4795_897E_16254776806A__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// EmacsEdit.h : header file
//
#include <list>
using std::list;
#include <string>
using std::string;
#include "simplewnd.h"
/////////////////////////////////////////////////////////////////////////////
// CEmacsEdit window
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


class CEmacsEdit : public CEdit
{
// Construction
public:
	CEmacsEdit();

// Attributes
public:

// Operations
public:
	void weVeMoved();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEmacsEdit)
	public:
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CEmacsEdit();

	int setHistFile(const CString& strFileName);
	void createListBox();
	void setListBox(CListBox& listBox);


	
private:
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
	CListBox* m_listBox;
	CSimpleWnd* m_simpleWnd;
	UINT m_id;
	list<string> m_histList;
	CString m_strHistFile;
	void saveHist();
	string getSelected();
	void SetWindowText(const string& str);
	void SetWindowText(const CString& str);
	// Generated message map functions
protected:
	//{{AFX_MSG(CEmacsEdit)
	afx_msg BOOL OnChange();
	afx_msg void OnKillFocus(CWnd* pNewWnd);
	afx_msg void OnSetFocus(CWnd* pOldWnd);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EMACSEDIT_H__5046851A_1A40_4795_897E_16254776806A__INCLUDED_)
