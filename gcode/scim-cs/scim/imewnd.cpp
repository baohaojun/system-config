#include "imewnd.h"
#include "imedefs.h"
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 
#include <Psapi.h>
#pragma comment(lib, "psapi")

void debug_rect(const CRect& rect)
{
	BHJDEBUG(" rect.left is %d", rect.left);
	BHJDEBUG(" rect.top is %d", rect.top);
	BHJDEBUG(" rect.width is %d", rect.Width());
	BHJDEBUG(" rect.height is %d", rect.Height());	
}

void FillSolidRect(HDC hdc, const CRect& rect, COLORREF rgb)
{
	HBRUSH hbr = CreateSolidBrush(rgb);
	::FillRect(hdc, &rect, hbr);
	DeleteObject(hbr);
}

input_context::input_context(HIMC himc, LPTRANSMSG msg_buf, u32 msg_buf_size) : 
	m_himc(himc), 
	m_ic(NULL),
	m_msg_buf(msg_buf),
	m_msg_buf_size(msg_buf_size),
	m_num_msg(0),
	m_hUIWnd(0)
{
	if (himc) {
		m_ic = (LPINPUTCONTEXT)ImmLockIMC(himc);
	} 
}

input_context::input_context(HWND hUIWnd) :
	m_himc(NULL),
	m_ic(NULL),
	m_msg_buf(NULL),
	m_msg_buf_size(0),
	m_num_msg(0),
	m_hUIWnd(NULL)
{
	if (hUIWnd) {
		m_hUIWnd = hUIWnd;
		m_himc = (HIMC)GetWindowLongPtr(hUIWnd, IMMGWLP_IMC);
	}
	if (m_himc) {
		m_ic = (LPINPUTCONTEXT)ImmLockIMC(m_himc);
	}
}

class trans_msg
{
public:
	trans_msg(input_context& ic);
	~trans_msg();
	 operator LPTRANSMSG () {
		return m_trans_msg_ptr;
	}

private:
	LPTRANSMSG m_trans_msg_ptr;
	input_context* m_ic_ptr;
};

trans_msg::trans_msg(input_context& ic) :
	m_ic_ptr(&ic),
	m_trans_msg_ptr(NULL)
{
	if (ic->hMsgBuf) {
		m_trans_msg_ptr = (LPTRANSMSG)ImmLockIMCC(ic->hMsgBuf);
	}
}

trans_msg::~trans_msg()
{
	if (m_trans_msg_ptr) {
		ImmUnlockIMCC( (*m_ic_ptr)->hMsgBuf);
	}
}

bool input_context::copy_old_msg()
{
	LPTRANSMSG msg_buf = trans_msg(*this);
	if (!msg_buf) {
		return false;
	}
	memcpy(msg_buf + m_ic->dwNumMsgBuf, m_msg_buf, m_num_msg*sizeof(TRANSMSG));
	return true;
}

bool input_context::enlarge_msg_buf(u32 n)
{
	HIMCC hMem;

	if (m_num_msg < m_msg_buf_size) { // this function must not be called when there's still space 
		bhjerr(" Error: enlarge_msg_buf called when still have space!");
	}

	if (m_num_msg == m_msg_buf_size) { // this should be the first time we are called
		n += m_num_msg;
	}

	if (!m_ic->hMsgBuf) {
		m_ic->dwNumMsgBuf = 0; 

		m_ic->hMsgBuf = ImmCreateIMCC( (n + m_num_msg) * sizeof(TRANSMSG));
		if (m_ic->hMsgBuf) {

			if (!copy_old_msg()) {
				return false;
			}
			m_ic->dwNumMsgBuf = n;
			return true;
		} else {
			return false;
		}
	} 
	
	if (hMem = ImmReSizeIMCC(m_ic->hMsgBuf,
									(m_ic->dwNumMsgBuf + n) * sizeof(TRANSMSG))) {
		if (hMem != m_ic->hMsgBuf) {
			ImmDestroyIMCC(m_ic->hMsgBuf);
			m_ic->hMsgBuf = hMem;
		}

		if (m_num_msg == m_msg_buf_size && m_num_msg > 0) { // copy the old messages over
			if (!copy_old_msg()) {
				return false;
			}
			m_num_msg += m_ic->dwNumMsgBuf; //number of valid messages
		}
		m_ic->dwNumMsgBuf += n; //this last n messages are not valid yet!
		return true;
	} else { // resize failed
		return false;
	}
}

bool input_context::add_show_comp_msg()
{
	return add_msg(WM_IME_NOTIFY, IMN_PRIVATE, 0);
}

bool input_context::add_update_status_msg()
{
	return add_msg(WM_IME_NOTIFY, IMN_OPENSTATUSWINDOW, 0);
}
bool input_context::add_msg(u32 msg, WPARAM wp, LPARAM lp)
{
	if (m_num_msg < m_msg_buf_size) {
		m_msg_buf[m_num_msg].message = msg;
		m_msg_buf[m_num_msg].wParam = wp;
		m_msg_buf[m_num_msg].lParam = lp;
		m_num_msg++;
		return true;
	}

	// we need to use the hMsgBuf
	if (!m_ic) {
		return false;
	}


	if (!enlarge_msg_buf(1)) {
		return false;
	}

	LPTRANSMSG lpTransMsg = trans_msg(*this);
	if (!lpTransMsg) {
		return false;
	}

	lpTransMsg[m_ic->dwNumMsgBuf - 1].message = msg;
	lpTransMsg[m_ic->dwNumMsgBuf - 1].wParam = wp;
	lpTransMsg[m_ic->dwNumMsgBuf - 1].lParam = lp;
	m_num_msg++;
	return true;
}

string g_comp_str;
string g_cands_str;
string g_hint_str;
string g_cand_idx_str;

wstring to_wstring(const string& str)
{
	WCHAR tmp;
	int n = MultiByteToWideChar(CP_UTF8, 
								0,
								str.c_str(),
								str.size(), 
								&tmp, 
								0);

	if (n == 0) {
		return L"Error: MultiByteToWideChar";
	}

	WCHAR* buf = (WCHAR*)malloc((n+1) * sizeof(WCHAR));
	if (!buf) {
		return L"";
	}
	MultiByteToWideChar(CP_UTF8, 
						0,
						str.c_str(), 
						str.size(),
						buf,
						n+1);
	buf[n] = 0;
	wstring wstr = buf;
	free(buf);
	return wstr;
}

string to_string(const wstring& wstr)
{
	if (wstr.empty()) {
		return "";
	}

	int n = WideCharToMultiByte(CP_UTF8, 0, wstr.c_str(), wstr.size(), NULL, 0, NULL, NULL);
	if (n == 0) {
		return "Error: WideCharToMultiByte";
	}
	char *buf = (char*)malloc((n+1) * sizeof(char));
	if (!buf) {
		return "Error: malloc";
	}
	WideCharToMultiByte(CP_UTF8, 0, wstr.c_str(), wstr.size(), buf, n+1, NULL, NULL);
	buf[n] = 0;
	string str = buf;
	free(buf);
	return str;
}

CRect get_wa_rect()
{
	CRect rect;
	SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0);
	return rect;
}

hdc_with_font::hdc_with_font(HDC hdc, wstring fnt_family, int size)
{
	LOGFONT lfFont;
	ZeroMemory(&lfFont, sizeof(lfFont));
	m_dc = hdc;
	m_old_font = (HFONT) GetCurrentObject(hdc, OBJ_FONT);
	lfFont.lfHeight = -MulDiv(size, GetDeviceCaps(hdc, LOGPIXELSY), 72);
	lfFont.lfCharSet = DEFAULT_CHARSET;
	lstrcpy(lfFont.lfFaceName, fnt_family.c_str());
	m_this_font = CreateFontIndirect(&lfFont); //delete at destructor
	SelectObject(hdc, m_this_font);
}

hdc_with_font::~hdc_with_font()
{
	SelectObject(m_dc, m_old_font);
	DeleteObject(m_this_font);
}

void hdc_with_font::use_this_font() //in case it is selected out
{
	SelectObject(m_dc, m_this_font);
}

void hdc_with_font::draw_text(const wstring& str, CRect& rect, colorref bg_color, bkmode bkm, colorref fg_color)
{
	use_this_font();
	
	if (bg_color.m_init) {
		COLORREF save = GetBkColor(m_dc);
		SetBkColor(m_dc, bg_color);
		bg_color = save;
	}

	if (fg_color.m_init) {
		COLORREF save = GetTextColor(m_dc);
		SetTextColor(m_dc, fg_color);
		fg_color = save;
	}

	if (bkm.m_init) {
		int save = GetBkMode(m_dc);
		SetBkMode(m_dc, bkm);
		bkm = save;
	}

	DrawText(m_dc, str.c_str(), str.size(), &rect, DT_VCENTER|DT_SINGLELINE);

	if (bg_color.m_init) {
		SetBkColor(m_dc, bg_color);
	}

	if (fg_color.m_init) {
		SetTextColor(m_dc, fg_color);
	}

	if (bkm.m_init) {
		SetBkMode(m_dc, bkm);
	}
}

void hdc_with_font::draw_text(const string& text, CRect& rect, colorref bg_color, bkmode bkm, colorref fg_color)
{
	draw_text(to_wstring(text), rect, bg_color, bkm, fg_color);
}

CSize hdc_with_font::get_text_size(const wstring& str)
{
	use_this_font();
	CSize size;
	GetTextExtentPoint(m_dc, str.c_str(), str.size(), &size);
	return size;
}

int hdc_with_font::get_text_width(const wstring& str)
{
	return get_text_size(str).cx;
}

int hdc_with_font::get_text_height(const wstring& str)
{
	return get_text_size(str).cy;
}

CSize hdc_with_font::get_text_size(const string& str)
{
	return get_text_size(to_wstring(str));
}

int hdc_with_font::get_text_width(const string& str)
{
	return get_text_width(to_wstring(str));
}

int hdc_with_font::get_text_height(const string& str)
{
	return get_text_height(to_wstring(str));
}

bool fill_result(input_context& ic, const wstring& wstr_result)
{
    HIMCC               hMem;
    LPCOMPOSITIONSTRING lpCompStr;
    DWORD               dwSize;

    dwSize = sizeof(COMPOSITIONSTRING) +  (wstr_result.size() + 1) * sizeof(WORD);

    if (!ic->hCompStr) {
        ic->hCompStr = ImmCreateIMCC(dwSize);
		if (!ic->hCompStr) {
			return false;
		}

		lpCompStr = (LPCOMPOSITIONSTRING) ImmLockIMCC(ic->hCompStr);
		if (!lpCompStr) {
			return false;
		}
		lpCompStr->dwSize = dwSize;
		ImmUnlockIMCC(ic->hCompStr);
	}

	lpCompStr = (LPCOMPOSITIONSTRING) ImmLockIMCC(ic->hCompStr);
	if (!lpCompStr) {
		return false;
	}

	if (dwSize > lpCompStr->dwSize) {
		ImmUnlockIMCC(ic->hCompStr);
		hMem = ImmReSizeIMCC(ic->hCompStr, dwSize);
		if (!hMem) {
			return false;
		}
		if (ic->hCompStr != hMem) {
			ImmDestroyIMCC(ic->hCompStr);
			ic->hCompStr = hMem;
		}

		lpCompStr = (LPCOMPOSITIONSTRING) ImmLockIMCC(ic->hCompStr);
		if (!lpCompStr) {
			return false;
		}
		lpCompStr->dwSize = dwSize;
	}

    dwSize = lpCompStr->dwSize; //save it

	memset(lpCompStr, 0, dwSize);
	lpCompStr->dwSize = dwSize;
	lpCompStr->dwResultStrLen = wstr_result.size();
	lpCompStr->dwResultStrOffset = sizeof(COMPOSITIONSTRING);
	memcpy((char *)lpCompStr+sizeof(COMPOSITIONSTRING), wstr_result.c_str(), wstr_result.size() * sizeof(wchar_t));

	ImmUnlockIMCC(ic->hCompStr);	
    return true;
}

int input_context::send_text(const string& str)
{
	wstring wstr = to_wstring(str);
	if (!fill_result(*this, wstr)) {
		return 0;
	}

	add_msg(WM_IME_COMPOSITION, 0, GCS_COMP|GCS_RESULT|GCS_RESULTREAD);
	add_show_comp_msg();
	return 2;
}

// int input_context::send_text(const string& str)
// {
// 	wstring wstr = to_wstring(str);
// 	for (size_t i = 0; i < wstr.size(); i++) {
// 		add_msg(WM_CHAR, wstr[i], 1);
// 	}
// 	return wstr.size();
// }

u32 input_context::return_ime_msgs()
{
	if (g_comp_str.empty()) {
		add_msg(WM_IME_ENDCOMPOSITION);
	}
	return m_num_msg;
}


ui_private_t* g_ui_private;


//To verify that some global var dtor will be called too early. Before DestroyUIWindow.
// class dood{
// public:
// 	dood() {};
// 	~dood();
// };
// dood::~dood()
// {
// 	int i=10;
// 	i*=5;
// 	int* j = &i;
// 	j+=i;
// }

// dood dx;

HWND get_comp_wnd(HWND hUIWnd)
{
	return (*g_ui_private)[hUIWnd].h_comp_wnd;
}
HWND get_status_wnd(HWND hUIWnd)
{
	return (*g_ui_private)[hUIWnd].h_stat_wnd;
}

void set_comp_wnd(HWND hUIWnd, HWND comp)
{
	(*g_ui_private)[hUIWnd].set_comp(comp);
}

void set_status_wnd(HWND hUIWnd, HWND stat)
{
	(*g_ui_private)[hUIWnd].set_stat(stat);
}


static wstring szUIClassName = L"BhjYwb";
static wstring szCompClassName = L"BhjYwbComp";
static wstring szStatusClassName = L"BhjYwbStatus";

static wstring get_moudle_name()
{
	static wstring module_name; //performance evil
	if (!module_name.empty()) {
		return module_name;
	}

	wchar_t buff[1024] = L"";
	GetModuleBaseName(GetCurrentProcess(), g_hInst, buff, 1024);
	string tmp = to_string(buff);
	module_name = to_wstring(tmp.substr(0, tmp.size()-4)); //remove the ".dll"
	
	return module_name;
}

wstring get_ui_class_name()
{
	return szUIClassName + get_moudle_name();
}

wstring get_comp_class_name()
{
	return szCompClassName + get_moudle_name();
}

wstring get_status_class_name()
{
	return szStatusClassName + get_moudle_name();
}

string string_format(const char* fmt, ...)
{
	int cap = 1024;
	char *buff = new char[cap];
	va_list ap;

	va_start(ap, fmt);
	for (;;) {
		int ret = vsnprintf(buff, cap-1, fmt, ap);
		if (ret < 0) {
			cap = int(cap*1.5);
			delete []buff;
			buff = new char[cap];
			continue;
		}
		buff[ret] = 0;
		break;
	}
	va_end(ap);

	string str = buff;
	delete []buff;
	return str;
}
