
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
#include <commctrl.h>
#include <map>
#include <sys/types.h>
#include <sys/stat.h>
#include <vector>
using std::vector;

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
	m_mark = -1;

}

CEkbEdit::~CEkbEdit()
{
}


BEGIN_MESSAGE_MAP(CEkbEdit, CEdit)
	//{{AFX_MSG_MAP(CEkbEdit)
	ON_CONTROL_REFLECT_EX(EN_CHANGE, OnChange)
	ON_WM_KILLFOCUS()
	ON_WM_SETFOCUS()
	ON_WM_CREATE()
	ON_WM_WINDOWPOSCHANGING()
	ON_WM_CTLCOLOR_REFLECT()
	ON_CONTROL_REFLECT(EN_HSCROLL, OnHscroll)
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


CRect GetWindowRect(CWnd* wnd)
{
	CRect rect;
	wnd->GetWindowRect(&rect);
	return rect;
}

CRect GetClientRect(CWnd* wnd)
{
	CRect rect;
	wnd->GetClientRect(&rect);
	return rect;
}

HWND getTopParentHwnd(CWnd* wnd)
{
	if (!wnd || !wnd->GetParentOwner()) {
		return NULL;
	}

	return wnd->GetParentOwner()->m_hWnd;
}

void CEkbEdit::selectPrevItem(int prev)
{
	if (!m_listBox) {
		return;
	}

	if (m_simpleWnd && !m_simpleWnd->IsWindowVisible()) {
		fillListBox(getText());
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
		return;
	}
	if (prev) {
		m_listBox->SetCurSel(m_listBox->GetCount()-1);
	} else {
		m_listBox->SetCurSel(0);
	}
}

void CEkbEdit::selectNextItem()
{
	selectPrevItem(0);
}

void CEkbEdit::move_to(int pos)
{
	if (pos < 0) {
		pos = 0;
	}
	
	if (pos > GetLength()) {
		pos = GetLength();
	}

	if (m_mark >= 0) {
		int start, end;
		GetSel(start, end);


		SetSel(m_mark, pos); 
	} else {
		CEdit::SetSel(pos, pos);
	}
}

void CEkbEdit::getTextFromSelectedItem()
{
	SetWindowText(getSelectedText());
	move_to(GetLength());
}

int CEkbEdit::getPoint()
{
	int start, end;
	CEdit::GetSel(start, end);
	if (start == end) {
		return start;
	}

	if (start == m_mark) {
		return end;
	} else {
		return start;
	}
}

void CEkbEdit::SetWindowText(const cstring& str)
{
	CWnd::SetWindowText(str.c_str());
	keyboard_quit();
	move_to(GetLength());
}

void CEkbEdit::SetWindowText(const CString& str)
{
	CWnd::SetWindowText((const char*)str);
}

void CEkbEdit::endOfLine()
{
	move_to(GetLength());
}

void CEkbEdit::beginOfLine()
{
	move_to(0);
}

void CEkbEdit::killEndOfLine()
{
	delete_range(getPoint(), GetLength());
}

void CEkbEdit::killBeginOfLine()
{
	delete_range(0, getPoint());
}

void CEkbEdit::forwardChar()
{

	if (getPoint() < GetLength()) {
		move_to(getPoint() + 1);
	}
}

void CEkbEdit::backwardChar()
{
	if (getPoint() > 0) {
		move_to(getPoint() - 1);
	}
}

int CEkbEdit::GetLength()
{
	return getText().size();
}

void CEkbEdit::deleteChar()
{
	int end = getPoint();
	if (end < GetLength()) {
		delete_range(end, end+1);
	}
}


void CEkbEdit::backwardWord()
{
	int start = getPoint();
	CString text;
	GetWindowText(text);
	enum {
		eInWord,
		eOutWord,
	};
	int state = eOutWord;
	for (int i=start-1; i>=0; i--) {
		if (state == eInWord && !isalnum(text[i])) {
			move_to(i+1);
			return;
		} else if (isalnum(text[i])) {
			state = eInWord;
		}
	}		
	move_to(0);
}

void CEkbEdit::SetSel(long start, long end)
{
	if (start < 0) {
		start = 0;
	}

	if (end < 0) {
		end = 0;
	}

	if (start > GetLength()) {
		start = GetLength();
	}
	if (end > GetLength()) {
		end = GetLength();
	}

	CEdit::SetSel(start, start);
	if (start == end) {
		return;
	}
   
	shift_move(start, end);
}

void CEkbEdit::shift_move(long start, long end)
{
	BHJDEBUG(" move from %d to %d", start, end);
	int nk = start>end ? start-end : end-start;

	BYTE kb[256];
	memset(kb, 0, 256);
	GetKeyboardState(kb);
	
	
	int spec_keys[] = {
		VK_LCONTROL,
		VK_LMENU,
		VK_LSHIFT,
		VK_LWIN,
		VK_RCONTROL,
		VK_RMENU,
		VK_RSHIFT,
		VK_RWIN,
		0,
	};

	typedef list<INPUT> vinput_t;
	vinput_t vi;
	vinput_t vi_cancel;


	for (int i=0; spec_keys[i]; i++) {
		int k = spec_keys[i];
		if (kb[k] & 0x80) {//this key is down
			INPUT input;

			memset(&input, 0, sizeof(input));
			input.type = INPUT_KEYBOARD;
			input.ki.wScan = ::MapVirtualKey(k, 0);  
			input.ki.dwFlags = KEYEVENTF_SCANCODE|KEYEVENTF_KEYUP;
			vi.push_back(input); //get it up

			input.ki.dwFlags = KEYEVENTF_SCANCODE;
			vi_cancel.push_front(input);
		}
	}

	if (kb[VK_NUMLOCK] & 0x1) {//numlock is on
		BHJDEBUG(" numlock is on");
		INPUT input;
		memset(&input, 0, sizeof(input));
		input.type = INPUT_KEYBOARD;
		input.ki.wScan = ::MapVirtualKey(VK_NUMLOCK, 0);
		input.ki.dwFlags = KEYEVENTF_SCANCODE;
		vi.push_back(input);
		input.ki.dwFlags = KEYEVENTF_SCANCODE|KEYEVENTF_KEYUP;
		vi.push_back(input);
	}


	for (i=0; i<nk; i++) {

		INPUT input;

		memset(&input, 0, sizeof(input));
		input.type = INPUT_KEYBOARD;
		input.ki.wScan = ::MapVirtualKey( VK_LSHIFT, 0 );  
		input.ki.dwFlags = KEYEVENTF_SCANCODE;
		vi.push_back(input);

		memset(&input, 0, sizeof(input));
		input.type = INPUT_KEYBOARD;
		input.ki.wVk = start>end ? VK_LEFT : VK_RIGHT;
		input.ki.dwFlags = 0;
		vi.push_back(input);

		memset(&input, 0, sizeof(input));
		input.type = INPUT_KEYBOARD;
		input.ki.wVk = start>end ? VK_LEFT : VK_RIGHT;
		input.ki.dwFlags = KEYEVENTF_KEYUP;
		vi.push_back(input);

		memset(&input, 0, sizeof(input));
		input.type = INPUT_KEYBOARD;
		input.ki.wScan = ::MapVirtualKey( VK_LSHIFT, 0 );
		input.ki.dwFlags = KEYEVENTF_KEYUP|KEYEVENTF_SCANCODE;
		vi.push_back(input);

	}




	INPUT *send = new INPUT[vi.size() + vi_cancel.size()];
	vinput_t::iterator vi_i;
	for (i=0, vi_i = vi.begin(); vi_i != vi.end(); i++, vi_i++) {
		send[i] = *vi_i;
	}

	for (vi_i = vi_cancel.begin(); vi_i != vi_cancel.end(); i++, vi_i++) {
		send[i] = *vi_i;
	}
	
	SendInput(vi.size() + vi_cancel.size(), send, sizeof(INPUT));
	delete []send;
}

void CEkbEdit::delete_range(int start, int end)
{
	keyboard_quit();
	CEdit::SetSel(start, end);
	Clear();
}

void CEkbEdit::backwardKillWord()
{
	int start = getPoint();
	CString text;
	GetWindowText(text);
	enum {
		eInWord,
		eOutWord,
	};
	int state = eOutWord;
	for (int i=start-1; i>=0; i--) {
		if (state == eInWord && !isalnum(text[i])) {
			delete_range(i+1, start);
			return;
		} else if (isalnum(text[i])) {
			state = eInWord;
		}
	}		
	delete_range(0, start);
}

void CEkbEdit::forwardWord()
{
	int end = getPoint();
	CString text;
	GetWindowText(text);
	enum {
		eInWord,
		eOutWord,
	};
	int state = eOutWord;
	for (int i=end+1; i<GetLength(); i++) {
		if (state == eInWord && !isalnum(text[i])) {
			move_to(i);
			return;
		} else if (isalnum(text[i])) {
			state = eInWord;
		}
	}
	move_to(GetLength());
}

void CEkbEdit::forwardKillWord()
{
	int end = getPoint();

	CString text;
	GetWindowText(text);
	enum {
		eInWord,
		eOutWord,
	};
	int state = eOutWord;
	for (int i=end+1; i<GetLength(); i++) {
		if (state == eInWord && !isalnum(text[i])) {
			delete_range(end, i);
			return;
		} else if (isalnum(text[i])) {
			state = eInWord;
		}
	}
	delete_range(end, GetLength());
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
	delete_range(0, GetLength());
	keyboard_quit();
	if (m_simpleWnd) {
		m_simpleWnd->hide();
	}
	if (m_listBox) {
		m_listBox->SetCurSel(-1);
	}
}

void CEkbEdit::set_mark_command()
{
	m_mark = getPoint();
}

void CEkbEdit::keyboard_quit()
{
	SetSel(getPoint(), getPoint());
	m_mark = -1;
}

void CEkbEdit::exchange_point_and_mark()
{
	int point = getPoint();
	int tmp = m_mark;
	m_mark = point;
	move_to(tmp);
}

void CEkbEdit::kill_region()
{
	Cut();
	keyboard_quit();
}

void CEkbEdit::kill_ring_save()
{
	Copy();
	keyboard_quit();
}

void CEkbEdit::yank()
{
	Paste();
	keyboard_quit();
}

bool want_debug_key(int vk)
{
	int ndks[] = {
		VK_LEFT,
		VK_RIGHT,
		VK_SHIFT,
		VK_CONTROL,
		VK_MENU,
		VK_LCONTROL, 
		VK_LSHIFT,
		VK_LMENU,
		0
	};
	for (int i=0; ndks[i]; i++) {
		if (vk == ndks[i]) {
			return false;
		}
	}
	return true;		
}

BOOL CEkbEdit::PreTranslateMessage(MSG* pMsg) 
{
	CString text;
	this->GetWindowText(text);
	char head=0,second=0;
	if(text.GetLength()>0) head=text.GetAt(0);
	if(text.GetLength()>1) second=text.GetAt(1);
	bool bCut=true;

	// if (pMsg->message == WM_KEYDOWN || pMsg->message == WM_SYSKEYDOWN ||
	// 	pMsg->message == WM_KEYUP || pMsg->message == WM_SYSKEYUP) {
	// 	BYTE kb[256];
	// 	GetKeyboardState(kb);
	// 	for (int i=0; i<256; i++) {
	// 		printf("%d ", kb[i]);
	// 		if (i%16 == 0){
	// 			printf("\n");
	// 		}
	// 	}
	// 	printf("\n********************************\n");
	// 	fflush(stdout);
	// }

	// return CEdit::PreTranslateMessage(pMsg);

	if (pMsg->message != WM_KEYDOWN && pMsg->message != WM_SYSKEYDOWN) {
		return CEdit::PreTranslateMessage(pMsg);
	}

	int debug_key = 0;
#define HandleKey(key, spec, handler) do {							\
		if (!debug_key && want_debug_key(pMsg->wParam)) {			\
			BHJDEBUG(" key is %d, spec is %d",						\
					 pMsg->wParam, getSpecKeyState());				\
		}															\
		debug_key = 1;												\
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
	HandleKey(VK_HOME, eNone, beginOfLine);
	HandleKey(VK_END, eNone, endOfLine);
	HandleKey('A', eCtrl, beginOfLine);
	HandleKey('X', eCtrl, exchange_point_and_mark);
	HandleKey('U', eCtrl, killBeginOfLine);
	HandleKey('K', eCtrl, killEndOfLine);
	HandleKey('F', eCtrl, forwardChar);
	HandleKey('B', eCtrl, backwardChar);
	HandleKey('D', eCtrl, deleteChar);
	HandleKey('B', eAlt, backwardWord);
	HandleKey('F', eAlt, forwardWord);
	HandleKey('D', eAlt, forwardKillWord);
	HandleKey('G', eCtrl, keyboard_quit);
	HandleKey('W', eCtrl, kill_region);
	HandleKey('W', eAlt, kill_ring_save);
	HandleKey('Y', eCtrl, yank);

	
	HandleKey(VK_SPACE, eCtrl, set_mark_command);
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

cstring CEkbEdit::getText()
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

lstring_t CEkbEdit::getMatchingStrings(const cstring& text)
{
	lstring_t ls_match;
	for (lstring_t::iterator i = m_histList.begin(); i != m_histList.end(); i++) {
		if (fields_match(*i, text)) {
			ls_match.push_back(*i);
		}
	}


	if (bce_dirname(text).c_str()[0] == '.') {
		return ls_match;
	}

	struct _stat stat;
	if (is_abspath(text) && 
		(_stat(bce_dirname(text), &stat) == 0) &&
		(stat.st_mode|_S_IFDIR)) {

		BHJDEBUG("%s is a dir %s", text.c_str(), bce_dirname(text).c_str());
		lstring_t files = getMatchingFiles(bce_dirname(text), bce_basename(text));
		if (!files.empty()) {
			ls_match.push_back("****************");
			ls_match.insert(ls_match.end(), files.begin(), files.end());
		}
	}
						   
		
	return ls_match;
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

	
	lstring_t ls_match = getMatchingStrings(text);
	for (lstring_t::iterator i = ls_match.begin(); i != ls_match.end(); i++) {
		m_listBox->AddString(CString(cstring(*i)));
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
	Invalidate(false);
	if ((m_simpleWnd && (CWnd*)m_simpleWnd == pNewWnd) || 
		(m_listBox && (CWnd*)m_listBox == pNewWnd)) {
		return;
	}

	if (m_simpleWnd) {
		//m_simpleWnd->ShowWindow(SW_HIDE);
	}
	
}

void CEkbEdit::OnSetFocus(CWnd* pOldWnd) 
{
	CEdit::OnSetFocus(pOldWnd);
	//weVeMoved();
	//HideCaret();
	//Invalidate(false);
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

static HWND newWindow(cstring wc_name, HWND h_owner=NULL)
{
	RegisterClass(wc_name);
	return CreateWindow (wc_name,                  // window class name
						 "",
						 WS_POPUP|WS_CLIPCHILDREN,
						 CW_USEDEFAULT,              // initial x position
						 CW_USEDEFAULT,              // initial y position
						 CW_USEDEFAULT,              // initial x size
						 CW_USEDEFAULT,              // initial y size
						 h_owner,                       // parent window handle
						 NULL,                       // window menu handle
						 AfxGetInstanceHandle(),                  // program instance handle
						 NULL) ;                     // creation parameters
}

LOGFONT getLogFont(CFont* font)
{
	LOGFONT lfont;
	font->GetLogFont(&lfont);
	return lfont;
}

CEkbHistWnd::CEkbHistWnd(CEdit* master)
{
	m_master = master;
	
	HWND hwnd = newWindow ("CEkbHistWnd", getTopParentHwnd(master));

	SubclassWindow(hwnd);
	CFont* font = m_master->GetFont();
	BHJDEBUG(" face name is %s", getLogFont(font).lfFaceName);
	ModifyStyleEx(0, WS_EX_TOOLWINDOW);
	m_listBox = new CHListBox();
	
	m_listBox->Create(WS_VSCROLL|WS_HSCROLL|LBS_NOTIFY|LBS_NOINTEGRALHEIGHT, CRect(0, 0, 1, 1), this, 0);
	m_listBox->SetFont(font);
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
	CRect rect = ::GetClientRect(this);
	dc.FillSolidRect(&rect, RGB(0, 0, 0));
	
	
}
/////////////////////////////////////////////////////////////////////////////
// CBalloon

CBalloon* CBalloon::getInstance(CWnd *owner)
{
	static map<CWnd*, CBalloon*> owner_map;

	if (!owner_map[owner]) {
		owner_map[owner] = new CBalloon(owner);
		//owner_map[owner]->SetFont(owner->GetFont());
		//BHJDEBUG(" balloon face name is %s", getLogFont(owner_map[owner]->GetFont()).lfFaceName);
	}
	return owner_map[owner];
}

CBalloon::CBalloon(CWnd* owner)
{
	HWND hwnd = newWindow ("CBalloon", getTopParentHwnd(owner));

	SubclassWindow(hwnd);
	LOGFONT lf = getLogFont(owner->GetFont());
	m_font.CreateFontIndirect(&lf);
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
	
	CRect rect = ::GetClientRect(this);
	rect.DeflateRect(1, 1);

	m_listBox->MoveWindow(rect.left, rect.top, rect.Width(), rect.Height());
	
}

void CBalloon::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CWnd::OnShowWindow(bShow, nStatus);
}

void CBalloon::showBalloon(CRect rect, const cstring& text)
{
	m_text = text;
	LONG cx = getTextWidth(text);
	rect.InflateRect(m_border + (cx-rect.Width())/2, 0);
	SetWindowPos(&wndTop, rect.left, rect.top, rect.Width(), rect.Height(), SWP_NOACTIVATE); 
	ShowWindow(SW_SHOWNA);
	UpdateWindow();
}

void CBalloon::OnPaint() 
{
	CPaintDC dc(this); // device context for painting

	CRect rect = ::GetClientRect(this);
	dc.FillSolidRect(&rect, RGB(10, 36, 106));	
	dc.SetTextColor(RGB(255, 255, 255));
	dc.SelectObject(&m_font);
	dc.TextOut(m_border, 0, m_text);
}

/////////////////////////////////////////////////////////////////////////////
// CHListBox

CHListBox::CHListBox()
{
 width = 0;
}

CHListBox::~CHListBox()
{
}


BEGIN_MESSAGE_MAP(CHListBox, CListBox)
	//{{AFX_MSG_MAP(CHListBox)
	ON_CONTROL_REFLECT(LBN_SELCHANGE, OnSelchange)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CHListBox message handlers
void CHListBox::updateWidth(LPCTSTR s)
{
	CClientDC dc(this);
	CFont * f = CListBox::GetFont();
	dc.SelectObject(f);
	CSize sz = dc.GetTextExtent(s, _tcslen(s));
	sz.cx += 3 * ::GetSystemMetrics(SM_CXBORDER);
	if(sz.cx > width)
	{ /* extend */
		width = sz.cx;
		CListBox::SetHorizontalExtent(width);
	} /* extend */
}

int CHListBox::AddString(LPCTSTR s)
{
	int result = CListBox::AddString(s);
	if(result < 0)
		return result;
	updateWidth(s);
	return result;
}

int CHListBox::InsertString(int i, LPCTSTR s)
{
	int result = CListBox::InsertString(i, s);
	if(result < 0)
		return result;
	updateWidth(s);
	return result;
}

void CHListBox::ResetContent()
{
	CListBox::ResetContent();
	width = 0;
}

int CHListBox::DeleteString(int n)
{
	int result = CListBox::DeleteString(n);
	if(result < 0)
		return result;
	CClientDC dc(this);

	CFont * f = CListBox::GetFont();
	dc.SelectObject(f);

	width = 0;
	for(int i = 0; i < CListBox::GetCount(); i++)
	{ /* scan strings */
		CString s;
		CListBox::GetText(i, s);
		CSize sz = dc.GetTextExtent(s);
		sz.cx += 3 * ::GetSystemMetrics(SM_CXBORDER);
		if(sz.cx > width)
			width = sz.cx;
	} /* scan strings */
	CListBox::SetHorizontalExtent(width);
	return result;
}

int CHListBox::SetCurSel(int nSelect)
{
	int ret = CListBox::SetCurSel(nSelect);
	OnSelchange();

	return ret;
}

cstring CHListBox::getSelectedText()
{
	if (GetCurSel() < 0) {
		return "";
	}

	int i = GetCurSel();
	CString text;
	GetText(i, text);
	return text;
}

CRect CHListBox::GetItemRect(int idx)
{
	CRect rect;
	CListBox::GetItemRect(idx, &rect);
	return rect;
}

CRect CHListBox::getSelectedRect()
{
	if (GetCurSel()<0) {
		return ::GetWindowRect(this);
	}
	
	CRect rect = GetItemRect(GetCurSel());
	CRect lbRect = ::GetWindowRect(this);
	
	rect.OffsetRect(lbRect.left, lbRect.top);
	return rect;
}


void CHListBox::OnSelchange() 
{
		CClientDC dc(this);
	CFont * f = GetFont();
	dc.SelectObject(f);
	CSize sz = dc.GetTextExtent(getSelectedText());
	sz.cx += 3 * ::GetSystemMetrics(SM_CXBORDER);
	if (sz.cx > ::GetClientRect(this).Width()) {
		getBalloon(this)->showBalloon(getSelectedRect(), getSelectedText());
	} else {
		getBalloon(this)->ShowWindow(SW_HIDE);
	}
}

CSize CBalloon::getTextSize(cstring text)
{
	CClientDC dc(this);
	dc.SelectObject(&m_font);
	CSize sz = dc.GetTextExtent(text);
	return sz;
}

LONG CBalloon::getTextWidth(cstring str)
{
	CSize sz = getTextSize(str);
	return sz.cx;
}

CSize CEkbEdit::getTextSize(cstring text)
{
	CClientDC dc(this);
	dc.SelectObject(GetFont());
	CSize sz = dc.GetTextExtent(text);
	return sz;
}

LONG CEkbEdit::getTextWidth(cstring str)
{
	CSize sz = getTextSize(str);
	return sz.cx;
}

LONG CEkbEdit::getTextHeight(cstring str)
{
	CSize sz = getTextSize(str);
	return sz.cy;
}

int CEkbEdit::CharFromPos(CPoint p)
{
	return CEdit::CharFromPos(p);
}

CPoint CEkbEdit::PosFromChar(int point)
{
	if (point >= GetLength()) {
		int display_start = CharFromPos(CPoint(1, 1));
		
		cstring display_str = getSubText(display_start, GetLength());
		int display_width = getTextWidth(display_str);
		BHJDEBUG(" display_width is %d", display_width);
		return CPoint(1+display_width, 1);
	}
	return CEdit::PosFromChar(point);
}


cstring CEkbEdit::getSubText(int start, int end)
{

	//range of start: [ 0, GetLength()-1 ]
	//range of end:   [ 0, GetLength()   ]
	if (start > GetLength() - 1) {
		start = GetLength() - 1;
	}

	if (end > GetLength()) {
		end = GetLength();
	}

	start = start < 0 ? 0 : start;
	end = end < 0 ? 0 : end;
	
	
	if (start >= end) {
		return "";
	}

	return getText().substr(start, end-start);	
}

int CEkbEdit::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CEdit::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	return 0;
}

void CEkbEdit::OnWindowPosChanging(WINDOWPOS FAR* lpwndpos) 
{
	CEdit::OnWindowPosChanging(lpwndpos);
	// TODO: Add your message handler code here
	
}

HBRUSH CEkbEdit::CtlColor(CDC* pDC, UINT nCtlColor) 
{
	return NULL;
}

void CEkbEdit::OnHscroll() 
{
	BHJDEBUG(" OnHscroll");
}
