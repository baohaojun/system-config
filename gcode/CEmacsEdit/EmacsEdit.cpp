// EmacsEdit.cpp : implementation file
//

#include "stdafx.h"
#include "alchemy.h"
#include "EmacsEdit.h"
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 
#include <shlobj.h>
#include "simplewnd.h"
#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CEmacsEdit

CEmacsEdit::CEmacsEdit()
{
	m_listBox = NULL;
	m_simpleWnd = NULL;
	m_id = 0;
	m_strHistFile = "";

}

CEmacsEdit::~CEmacsEdit()
{
}


BEGIN_MESSAGE_MAP(CEmacsEdit, CEdit)
	//{{AFX_MSG_MAP(CEmacsEdit)
	ON_CONTROL_REFLECT_EX(EN_CHANGE, OnChange)
	ON_WM_KILLFOCUS()
	ON_WM_SETFOCUS()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEmacsEdit message handlers

specKeyState_t CEmacsEdit::getSpecKeyState()
{
	int skState = eNone;
	if (GetKeyState(VK_CONTROL)<0) {
		skState |= eCtrl;
	}

	if (GetKeyState(VK_MENU) < 0) {
		skState |= eAlt;
	}

	if (GetKeyState(VK_SHIFT) < 0) {
		skState |= eShift;
	}

	return (specKeyState_t) skState;
}

void CEmacsEdit::setListBox(CListBox& listBox)
{
	if (m_simpleWnd) {
		return;
	}
	m_listBox = &listBox;
}

void CEmacsEdit::createListBox()
{
	if (m_listBox) {
		BHJDEBUG(" already has a listbox");
		return;
	}

	m_simpleWnd = new CSimpleWnd(this);

	m_listBox = m_simpleWnd->m_listBox;
}

void CEmacsEdit::selectNextItem()
{
	selectPrevItem(0);
}

void CEmacsEdit::selectPrevItem(int prev)
{
	if (!m_listBox) {
		return;
	}

	if (m_simpleWnd) {
		m_simpleWnd->show();
	}

	if (m_listBox->GetCount() == 0) {
		return;
	}

	for (int i = 0; i<m_listBox->GetCount(); i++) {
		if (m_listBox->GetSel(i)) {
			m_listBox->SetSel(-1, false);
			if (prev) {
				i = (i + m_listBox->GetCount() - 1) % m_listBox->GetCount();
			} else {
				i = (i+1) % m_listBox->GetCount();
			}		
			int ret = m_listBox->SetSel(i, true);
			return;
		}
	}
	if (prev) {
		m_listBox->SetSel(m_listBox->GetCount()-1, true);
	} else {
		m_listBox->SetSel(0, true);
	}
}

void CEmacsEdit::getTextFromSelectedItem()
{
	if (!m_listBox || !m_listBox->GetCount()) {
		return;
	}
	for (int i=0; i<m_listBox->GetCount(); i++) {
		if (m_listBox->GetSel(i)) {
			CString str;
			m_listBox->GetText(i, str);
			SetWindowText(str);
			return;
		}
	}	
}

void CEmacsEdit::endOfLine()
{
	CString text;
	GetWindowText(text);
	SetSel(text.GetLength(), text.GetLength());
}

void CEmacsEdit::beginOfLine()
{
	SetSel(0, 0);
}

void CEmacsEdit::killEndOfLine()
{
	int start, end;
	GetSel(start, end);
	CString text;
	GetWindowText(text);
	SetSel(start, text.GetLength());
	Clear();
}

void CEmacsEdit::killBeginOfLine()
{
	int start, end; 
	GetSel(start, end);
	SetSel(0, end);
	Clear();
}

void CEmacsEdit::forwardChar()
{
	int start, end;
	GetSel(start, end);
	CString text;
	GetWindowText(text);
	if (end < text.GetLength()) {
		end ++;
	}
	SetSel(end, end);	
}

void CEmacsEdit::backwardChar()
{
	int start, end;
	GetSel(start, end);
	if (start > 0) {
		start --;
	}

	SetSel(start, start);
}

int CEmacsEdit::GetLength()
{
	CString text;
	GetWindowText(text);
	return text.GetLength();
}

void CEmacsEdit::deleteChar()
{
	int start, end;
	GetSel(start, end);
	if (end < GetLength()) {
		SetSel(end, end+1);
		Clear();
	}
}


void CEmacsEdit::backwardWord()
{
	int start, end;
	GetSel(start, end);
	CString text;
	GetWindowText(text);
	enum {
		eInWord,
		eOutWord,
	};
	int state = eOutWord;
	for (int i=start-1; i>=0; i--) {
		if (state == eInWord && !isalnum(text[i])) {
			SetSel(i+1, i+1);
			return;
		} else if (isalnum(text[i])) {
			state = eInWord;
		}
	}		
	SetSel(0, 0);
}
void CEmacsEdit::backwardKillWord()
{
	int start, end;
	GetSel(start, end);
	CString text;
	GetWindowText(text);
	enum {
		eInWord,
		eOutWord,
	};
	int state = eOutWord;
	for (int i=start-1; i>=0; i--) {
		if (state == eInWord && !isalnum(text[i])) {
			SetSel(i+1, start);
			Clear();
			return;
		} else if (isalnum(text[i])) {
			state = eInWord;
		}
	}		
	SetSel(0, start);
	Clear();
}

void CEmacsEdit::forwardWord()
{
	int start, end;
	GetSel(start, end);
	CString text;
	GetWindowText(text);
	enum {
		eInWord,
		eOutWord,
	};
	int state = eOutWord;
	for (int i=end+1; i<GetLength(); i++) {
		if (state == eInWord && !isalnum(text[i])) {
			SetSel(i, i);
			return;
		} else if (isalnum(text[i])) {
			state = eInWord;
		}
	}
	SetSel(GetLength(), GetLength());
}

void CEmacsEdit::forwardKillWord()
{
	int start, end;
	GetSel(start, end);

	CString text;
	GetWindowText(text);
	enum {
		eInWord,
		eOutWord,
	};
	int state = eOutWord;
	for (int i=end+1; i<GetLength(); i++) {
		if (state == eInWord && !isalnum(text[i])) {
			SetSel(end, i);
			Clear();
			return;
		} else if (isalnum(text[i])) {
			state = eInWord;
		}
	}
	SetSel(end, GetLength());
	Clear();
}

CString CEmacsEdit::getSelected()
{
	if (!m_listBox) {
		return "";
	}

	
	for (int i=0; i<m_listBox->GetCount(); i++) {
		if (m_listBox->GetSel(i)) {
			CString text;
			m_listBox->GetText(i, text);
			return text;
		}
	}
	return "";
}

void CEmacsEdit::escapeEdit()
{
	SetSel(0, GetLength());
	Clear();
	if (m_simpleWnd) {
		m_simpleWnd->hide();
	}
}

BOOL CEmacsEdit::PreTranslateMessage(MSG* pMsg) 
{
	// TODO: Add your specialized code here and/or call the base class

	// TODO: Add your specialized code here and/or call the base class
	int start,end;
	GetSel(start,end);
	CString text;
	this->GetWindowText(text);
	char head=0,second=0;
	if(text.GetLength()>0) head=text.GetAt(0);
	if(text.GetLength()>1) second=text.GetAt(1);
	bool bCut=true;

	if (pMsg->message != WM_KEYDOWN && pMsg->message != WM_SYSKEYDOWN) {
		return CEdit::PreTranslateMessage(pMsg);
	}

#define HandleKey(key, spec, handler) do {							\
		if (pMsg->wParam == (key) && getSpecKeyState() == (spec)) {	\
			handler();												\
			return true;											\
		}															\
	} while (0);


	HandleKey('N', eCtrl, selectNextItem);
	HandleKey('P', eCtrl, selectPrevItem);
	HandleKey(VK_DOWN, eNone, selectNextItem);
	HandleKey(VK_UP, eNone, selectPrevItem);
	HandleKey(VK_RETURN, eCtrl, getTextFromSelectedItem);
	HandleKey('E', eCtrl, endOfLine);
	HandleKey('A', eCtrl, beginOfLine);
	HandleKey('U', eCtrl, killBeginOfLine);
	HandleKey('K', eCtrl, killEndOfLine);
	HandleKey('F', eCtrl, forwardChar);
	HandleKey('B', eCtrl, backwardChar);
	HandleKey('D', eCtrl, deleteChar);
	HandleKey('B', eAlt, backwardWord);
	HandleKey('F', eAlt, forwardWord);
	HandleKey('D', eAlt, forwardKillWord);
	HandleKey(VK_BACK, eAlt, backwardKillWord);
	HandleKey(VK_ESCAPE, eNone, escapeEdit);


	if (pMsg->wParam == VK_RETURN && getSpecKeyState() == eNone && getSelected().GetLength()) {
		BHJDEBUG(" in vk_return");
		SetWindowText(getSelected());
		m_histList.push_back(getSelected());
		m_histList.sort();
		m_histList.unique();
		saveHist();
		fillListBox("");
		if (m_simpleWnd) {
			m_simpleWnd->hide();
		}
		return CEdit::PreTranslateMessage(pMsg);
	}
		
	return CEdit::PreTranslateMessage(pMsg);
}

CString CEmacsEdit::getText()
{
	CString text;
	GetWindowText(text);
	return text;
}

void CEmacsEdit::saveHist()
{
	if (m_strHistFile.GetLength() == 0) {
		return ;
	}

	FILE* fp = fopen(m_strHistFile, "wb");
	if (!fp) {
		return;
	}

	m_histList.sort();
	m_histList.unique();
	for (tStrList::iterator i = m_histList.begin(); i != m_histList.end(); i++) {
		fprintf(fp, "%s\n", *i);
	}
	fclose(fp);
}

BOOL CEmacsEdit::OnChange() 
{

	if (m_simpleWnd) {
		if (GetLength()) {
			m_simpleWnd->show();
		} else {
			m_simpleWnd->hide();
		}
	}


	fillListBox(getText());
	return false;
}

void CEmacsEdit::fillListBox(const CString& text)
{
	if (!m_listBox) {
		return;
	}
	
	m_listBox->ResetContent();
	m_listBox->AddString(text);
	m_histList.sort();
	m_histList.unique();
	for (tStrList::iterator i = m_histList.begin(); i != m_histList.end(); i++) {
		if (stringContains(*i, text)) {
			m_listBox->AddString(*i);
		}
	}

	if (m_listBox->GetCount()) {
		m_listBox->SetSel(0, true);
	}
}

int CEmacsEdit::setHistFile(const CString& strFileName)
{
// HRESULT SHGetFolderPath(          HWND hwndOwner,
//     int nFolder,
//     HANDLE hToken,
//     DWORD dwFlags,
//     LPTSTR pszPath
// );
	char strAppPath[MAX_PATH] = "";
	HRESULT ret = SHGetFolderPath(NULL, CSIDL_LOCAL_APPDATA, NULL, SHGFP_TYPE_CURRENT, strAppPath);
	if (ret != S_OK) {
		return -1;
	}

	m_strHistFile.Format("%s\\%s", strAppPath, strFileName);
	FILE* fp = fopen(m_strHistFile, "rb");
	if (!fp) {
		return -1;
	}

	char buff[2048];
	m_histList.clear();
	while (fgets(buff, 2048, fp)) { //the '\n' is in the buff!
		CStringArray strArr;
		stringSplit(buff, "\r", strArr);
		CString str = stringJoin("", strArr);
		stringSplit(str, '\n', strArr);
		m_histList.push_back(stringJoin("", strArr));
	}
	fclose(fp);
	m_histList.sort();
	m_histList.unique();
	return 0;
}

void CEmacsEdit::OnKillFocus(CWnd* pNewWnd) 
{
	CEdit::OnKillFocus(pNewWnd);
	
	if (m_simpleWnd) {
		m_simpleWnd->ShowWindow(SW_HIDE);
	}
	
}

void CEmacsEdit::OnSetFocus(CWnd* pOldWnd) 
{
	CEdit::OnSetFocus(pOldWnd);
	
	if (m_simpleWnd) {
		m_simpleWnd->ShowWindow(SW_SHOWNA);
		m_listBox->SetSel(0, true);
	}
	
}

void CEmacsEdit::weVeMoved()
{
	if (m_simpleWnd) {
		m_simpleWnd->weVeMoved();
	}
}
