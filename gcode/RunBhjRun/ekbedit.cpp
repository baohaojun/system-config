
// EkbEdit.cpp : implementation file
//

#include <list>
#include <string>
#include "stdafx.h"

#include "runbhjrun.h"

#include <list>
#include <string>
#include "EkbEdit.h"
#include "bhjlib.h"

#include <fstream>
#include <iostream>
#include <iterator>
#include <boost/regex.hpp>

using namespace boost;
#include <iostream>
#include <string>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 
#include "HListBox.h"
#include <commctrl.h>
#include <map>
using std::map;

using namespace bhj;
using std::list;


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CEkbEdit

CEkbEdit::CEkbEdit()
{
	m_listBox = NULL;
	m_simpleWnd = NULL;
	m_id = 0;
	m_strHistFile = "";

}

CEkbEdit::~CEkbEdit()
{
}


BEGIN_MESSAGE_MAP(CEkbEdit, CEdit)
	//{{AFX_MSG_MAP(CEkbEdit)
	ON_CONTROL_REFLECT_EX(EN_CHANGE, OnChange)
	ON_WM_KILLFOCUS()
	ON_WM_SETFOCUS()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEkbEdit message handlers

specKeyState_t CEkbEdit::getSpecKeyState()
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

void CEkbEdit::setListBox(CHListBox& listBox)
{
	if (m_simpleWnd) {
		return;
	}
	m_listBox = &listBox;
}

void CEkbEdit::createListBox()
{
	if (m_listBox) {
		BHJDEBUG(" already has a listbox");
		return;
	}

	m_simpleWnd = new CEkbHistWnd(this);

	m_listBox = m_simpleWnd->m_listBox;
}

void CEkbEdit::selectNextItem()
{
	selectPrevItem(0);
}

CRect CEkbEdit::GetClientRect()
{
	CRect rect;
	CEdit::GetClientRect(&rect);
	return rect;
}

void CEkbEdit::showBalloon()
{
	if (!m_listBox) {
		return;
	}
	CClientDC dc(m_listBox);
	CFont * f = m_listBox->GetFont();
	dc.SelectObject(f);
	CSize sz = dc.GetTextExtent(getSelectedText());
	sz.cx += 3 * ::GetSystemMetrics(SM_CXBORDER);
	BHJDEBUG(" text width is %d, edit width is %d", sz.cx, GetClientRect().Width());
	if (sz.cx > GetClientRect().Width()) {
		getBalloon()->showBalloon(GetParent(), getSelectedRect(), getSelectedText());
	} else {
		getBalloon()->ShowWindow(SW_HIDE);
	}

}
void CEkbEdit::selectPrevItem(int prev)
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

	if (m_listBox->GetCurSel() >= 0) {
		int i = m_listBox->GetCurSel();
		m_listBox->SetCurSel(-1);
		if (prev) {
			i = (i + m_listBox->GetCount() - 1) % m_listBox->GetCount();
		} else {
			i = (i+1) % m_listBox->GetCount();
		}		
		int ret = m_listBox->SetCurSel(i);
		showBalloon();
		return;
	}
	if (prev) {
		m_listBox->SetCurSel(m_listBox->GetCount()-1);
	} else {
		m_listBox->SetCurSel(0);
	}
}

void CEkbEdit::getTextFromSelectedItem()
{
	SetWindowText(getSelectedText());
}

void CEkbEdit::SetWindowText(const cstring& str)
{
	CWnd::SetWindowText(str.c_str());
}

void CEkbEdit::SetWindowText(const CString& str)
{
	CWnd::SetWindowText((const char*)str);
}
void CEkbEdit::endOfLine()
{
	CString text;
	GetWindowText(text);
	SetSel(text.GetLength(), text.GetLength());
}

void CEkbEdit::beginOfLine()
{
	SetSel(0, 0);
}

void CEkbEdit::killEndOfLine()
{
	int start, end;
	GetSel(start, end);
	CString text;
	GetWindowText(text);
	SetSel(start, text.GetLength());
	Clear();
}

void CEkbEdit::killBeginOfLine()
{
	int start, end; 
	GetSel(start, end);
	SetSel(0, end);
	Clear();
}

void CEkbEdit::forwardChar()
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

void CEkbEdit::backwardChar()
{
	int start, end;
	GetSel(start, end);
	if (start > 0) {
		start --;
	}

	SetSel(start, start);
}

int CEkbEdit::GetLength()
{
	CString text;
	GetWindowText(text);
	return text.GetLength();
}

void CEkbEdit::deleteChar()
{
	int start, end;
	GetSel(start, end);
	if (end < GetLength()) {
		SetSel(end, end+1);
		Clear();
	}
}


void CEkbEdit::backwardWord()
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
void CEkbEdit::backwardKillWord()
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

void CEkbEdit::forwardWord()
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

void CEkbEdit::forwardKillWord()
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

CRect CEkbEdit::getSelectedRect()
{
	CRect rect;
	if (!m_listBox || m_listBox->GetCurSel()<0) {
		GetWindowRect(&rect);
		return rect;
	}
	
	m_listBox->GetItemRect(m_listBox->GetCurSel(), &rect);
	CRect lbRect;
	m_listBox->GetWindowRect(&lbRect);
	
	rect.OffsetRect(lbRect.left, lbRect.top);
	return rect;
}

cstring CEkbEdit::getSelectedText()
{
	if (!m_listBox || !m_listBox->GetCount()) {
		return "";
	}

	if (m_listBox->GetCurSel() < 0) {
		return "";
	}

	int i = m_listBox->GetCurSel();
	CString text;
	m_listBox->GetText(i, text);
	return text;
}

void CEkbEdit::escapeEdit()
{
	SetSel(0, GetLength());
	Clear();
	if (m_simpleWnd) {
		m_simpleWnd->hide();
	}
	if (m_listBox) {
		m_listBox->SetCurSel(-1);
	}
}

BOOL CEkbEdit::PreTranslateMessage(MSG* pMsg) 
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
	} while (0)

#define HandleKeyIf(key, spec, handler, cond) do {					\
		if (pMsg->wParam == (key) && getSpecKeyState() == (spec)) {	\
			if (cond) {												\
				handler();											\
				return true;										\
			} else {												\
				return CEdit::PreTranslateMessage(pMsg);			\
			}														\
		}															\
	} while (0)


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
	HandleKey(VK_BACK, eCtrl, backwardKillWord);
	HandleKeyIf(VK_ESCAPE, eNone, escapeEdit, GetLength());


	if (pMsg->wParam == VK_RETURN && getSpecKeyState() == eNone && getSelectedText().size()) {
		BHJDEBUG(" in vk_return");
		SetWindowText(getSelectedText());

		m_histList.push_back(getSelectedText());
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

CString CEkbEdit::getText()
{
	CString text;
	GetWindowText(text);
	return text;
}

void CEkbEdit::saveHist()
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
	for (lstring_t::iterator i = m_histList.begin(); i != m_histList.end(); i++) {
		fprintf(fp, "%s\n", i->c_str());
	}
	fclose(fp);
}

BOOL CEkbEdit::OnChange() 
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

bool stringContains(const CString& src, const CString& tgt)
{
	return src.Find(tgt) >= 0;
}

void CEkbEdit::fillListBox(const CString& text)
{
	if (!m_listBox) {
		return;
	}
	
	m_listBox->ResetContent();
	m_listBox->AddString(text);
	m_histList.sort();
	m_histList.unique();

	
	for (lstring_t::iterator i = m_histList.begin(); i != m_histList.end(); i++) {
		if (fields_match(*i, text)) {
			m_listBox->AddString(CString(cstring(*i)));
		}
	}

	if (m_listBox->GetCount()) {
		m_listBox->SetCurSel(0);
	}
}

int CEkbEdit::setHistFile(const CString& strFileName)
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
		cstring str = buff;
		str = regex_replace(str, regex("\r|\n"), "", match_default|format_perl);
		m_histList.push_back(str);
	}
	fclose(fp);
	m_histList.sort();
	m_histList.unique();
	fillListBox("");
	return 0;
}

void CEkbEdit::OnKillFocus(CWnd* pNewWnd) 
{
	CEdit::OnKillFocus(pNewWnd);
	if ((m_simpleWnd && (CWnd*)m_simpleWnd == pNewWnd) || 
		(m_listBox && (CWnd*)m_listBox == pNewWnd)) {
		return;
	}
	
	if (m_simpleWnd) {
		m_simpleWnd->ShowWindow(SW_HIDE);
	}
	
}

void CEkbEdit::OnSetFocus(CWnd* pOldWnd) 
{
	CEdit::OnSetFocus(pOldWnd);
	weVeMoved();
	
	if (m_simpleWnd && GetLength()) {
		m_simpleWnd->ShowWindow(SW_SHOWNA);
		m_listBox->SetCurSel(0);
	}
	
}

void CEkbEdit::weVeMoved()
{
	if (m_simpleWnd) {
		m_simpleWnd->weVeMoved();
	}
}

/////////////////////////////////////////////////////////////////////////////
// CEkbEdit message handlers
/////////////////////////////////////////////////////////////////////////////
// CEkbHistWnd

static ATOM RegisterClass(cstring str)
{
	static map<cstring, ATOM> name_class_map;
	
	if (name_class_map.find(str) != name_class_map.end()) {
		return name_class_map[str];
	}
	
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
	wndclass.lpszClassName = str.c_str();

	ATOM atom;
	if (!(atom = RegisterClass (&wndclass)))
	{
		FmtMessageBox("Failed to register class for %s", str.c_str());
		exit(-1);
	}

	name_class_map[str] = atom;
	return atom;
}

static HWND newWindow(cstring wc_name)
{
	RegisterClass(wc_name);
	return CreateWindow (wc_name,                  // window class name
						 "",
						 WS_POPUP|WS_CLIPCHILDREN,
						 CW_USEDEFAULT,              // initial x position
						 CW_USEDEFAULT,              // initial y position
						 CW_USEDEFAULT,              // initial x size
						 CW_USEDEFAULT,              // initial y size
						 NULL,                       // parent window handle
						 NULL,                       // window menu handle
						 AfxGetInstanceHandle(),                  // program instance handle
						 NULL) ;                     // creation parameters
}

CEkbHistWnd::CEkbHistWnd(CEdit* master)
{
	m_master = master;
	
	HWND hwnd = newWindow ("CEkbHistWnd");

	SubclassWindow(hwnd);
	ModifyStyleEx(0, WS_EX_TOOLWINDOW);
	m_listBox = new CHListBox();
	
	m_listBox->Create(WS_VSCROLL|WS_HSCROLL|LBS_NOTIFY|LBS_NOINTEGRALHEIGHT, CRect(0, 0, 1, 1), this, 0);
	m_listBox->ShowWindow(SW_SHOWNA);
	m_listBox->UpdateWindow();

}

CEkbHistWnd::~CEkbHistWnd()
{
}


BEGIN_MESSAGE_MAP(CEkbHistWnd, CWnd)
	//{{AFX_MSG_MAP(CEkbHistWnd)
	ON_WM_SHOWWINDOW()
	ON_WM_PAINT()
	ON_WM_SIZE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CEkbHistWnd message handlers

void CEkbHistWnd::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CWnd::OnShowWindow(bShow, nStatus);
	CRect rect;
	calcWindowRect(rect);
	SetWindowPos(&wndTop, rect.left, rect.top, rect.Width(), rect.Height(), SWP_NOACTIVATE);
}

void CEkbHistWnd::calcWindowRect(CRect& rect)
{
	m_master->GetWindowRect(&rect);
	CRect tmpRect = rect;
	int top = rect.bottom + 2;
	int left = rect.left;

	rect.OffsetRect(0, top-tmpRect.top);
	rect.bottom += rect.Height()*9;

	RECT waRect;
	SystemParametersInfo(SPI_GETWORKAREA, 0, &waRect, 0);


	if (rect.bottom > waRect.bottom) {
		BHJDEBUG(" hello world");
		int bottom = tmpRect.top - 2;
		int height = rect.Height();
		rect.bottom = bottom;
		rect.top =bottom - height;
	}
}

void CEkbHistWnd::hide()
{
	ShowWindow(SW_HIDE);
}

void CEkbHistWnd::show()
{
	ShowWindow(SW_SHOWNA);
}

void CEkbHistWnd::weVeMoved()
{
	CRect rect;
	calcWindowRect(rect);
	SetWindowPos(&wndTop, rect.left, rect.top, rect.Width(), rect.Height(), SWP_NOACTIVATE);
}

void CEkbHistWnd::OnPaint() 
{
	CPaintDC dc(this); // device context for painting
	CRect rect;
	GetClientRect(&rect);
	dc.FillSolidRect(&rect, RGB(0, 0, 0));
	
	
}
/////////////////////////////////////////////////////////////////////////////
// CBalloon

CBalloon* CBalloon::getInstance()
{
	static CBalloon* theBalloon;
	if (!theBalloon) {
		theBalloon = new CBalloon();
	}
	return theBalloon;
}

CBalloon::CBalloon()
{
	HWND hwnd = newWindow ("CBalloon");

	SubclassWindow(hwnd);
	ModifyStyleEx(0, WS_EX_TOOLWINDOW);
}

CBalloon::~CBalloon()
{
}


BEGIN_MESSAGE_MAP(CBalloon, CWnd)
	//{{AFX_MSG_MAP(CBalloon)
	ON_WM_SHOWWINDOW()
	ON_WM_PAINT()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CBalloon message handlers

void CEkbHistWnd::OnSize(UINT nType, int cx, int cy) 
{
	CWnd::OnSize(nType, cx, cy);
	
	CRect rect;
	GetClientRect(&rect);
	rect.DeflateRect(1, 1);

	m_listBox->MoveWindow(rect.left, rect.top, rect.Width(), rect.Height());
	
}

void CBalloon::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CWnd::OnShowWindow(bShow, nStatus);
}

void CBalloon::showBalloon(CWnd* owner, CRect rect, const cstring& text)
{
	m_text = text;
	SetWindowPos(&wndTopMost, rect.left, rect.top, rect.Width(), rect.Height(), SWP_NOACTIVATE); 
	ShowWindow(SW_SHOWNA);
	UpdateWindow();
}

void CBalloon::OnPaint() 
{
	CPaintDC dc(this); // device context for painting

	CRect rect;
	GetClientRect(&rect);
	rect.InflateRect(-2, -2);
	dc.FillSolidRect(&rect, RGB(200, 200, 30));	
	// TODO: Add your message handler code here
	
	// Do not call CWnd::OnPaint() for painting messages CListBox
}
