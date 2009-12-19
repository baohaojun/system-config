#include "imewnd.h"
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 

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
	m_old_font = GetCurrentObject(hdc, OBJ_FONT);
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

void hdc_with_font::draw_text(const wstring& str, CRect& rect)
{
	use_this_font();
	DrawText(m_dc, str.c_str(), str.size(), &rect, DT_VCENTER|DT_SINGLELINE);
}

int hdc_with_font::get_text_width(const wstring& str)
{
	use_this_font();
	CSize size;
	GetTextExtentPoint(m_dc, str.c_str(), str.size(), &size);
	return size.cx;
	
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

	for (size_t i=0; i < wstr.size(); i++) {
		if (wstr[i] > 127) { //this is a non-ascii char, we want to make it into history
			g_history_list.push_back(wstr[i]);
			g_history_list.pop_front();
		}
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

void promote_cand_for_key(u32 cand_num, const string& key) //cand can't be passed as a ref, weird?
{

	if (!map_has_key(g_quail_rules, key)
		|| g_quail_rules[key].empty()) {
		return;
	}

	if (cand_num == 0 && !map_has_key(g_cand_hist, key)) { // no need to reorder;
		return;
	}

	g_cand_hist[key] = cand_num;

}

void self_make_cand_for_key(const string& cand, const string& key)
{
	if (key.size() != 4 || cand.empty()) {// we only want to re-order if key is a full key (4)
		return;
	}

	vector<string> &cands = g_quail_rules[key];
	int active = 0;
	for (vector<string>::iterator i = cands.begin(); i != cands.end(); i++, active++) {
		if (*i == cand) {
			g_cand_hist[key] = active;
			return;
		}
	}
	
	cands.push_back(cand);
	g_cand_hist[key] = cands.size() - 1;	
}

ui_private_t g_ui_private;

HWND get_comp_wnd(HWND hUIWnd)
{
	return g_ui_private[hUIWnd].h_comp_wnd;
}
HWND get_status_wnd(HWND hUIWnd)
{
	return g_ui_private[hUIWnd].h_stat_wnd;
}

void set_comp_wnd(HWND hUIWnd, HWND comp)
{
	g_ui_private[hUIWnd].set_comp(comp);
}
void set_status_wnd(HWND hUIWnd, HWND stat)
{
	g_ui_private[hUIWnd].set_stat(stat);
}


static wstring szUIClassName = L"BhjCsImeUI";
static wstring szCompClassName = L"BhjCsImeComp";
static wstring szStatusClassName = L"BhjCsImeStatus";

wstring get_ui_class_name()
{
	return szUIClassName + to_wstring(ime_off);
}

wstring get_comp_class_name()
{
	return szCompClassName + to_wstring(ime_off);
}

wstring get_status_class_name()
{
	return szStatusClassName + to_wstring(ime_off);
}
