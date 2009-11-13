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
	m_num_msg(0)
{
	if (himc) {
		m_ic = (LPINPUTCONTEXT)ImmLockIMC(himc);
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
	if (m_num_msg == m_msg_buf_size && m_num_msg > 0) { // copy the old messages over
		LPTRANSMSG msg_buf = trans_msg(*this);
		if (!msg_buf) {
			return false;
		}
		memcpy(msg_buf + m_ic->dwNumMsgBuf, m_msg_buf, m_num_msg*sizeof(TRANSMSG));
	}
	return true;
}

bool input_context::enlarge_msg_buf(u32 n)
{
	HIMCC hMem;

	if (m_num_msg < m_msg_buf_size) { // this function must not be called when there's still space 
		BHJDEBUG(" Error: enlarge_msg_buf called when still have space!");
		exit(-1);
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

		if (!copy_old_msg()) {
			return false;
		}
		m_ic->dwNumMsgBuf += n; //this last n messages are not valid yet!
		return true;
	} else { // resize failed
		return false;
	}
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

	BHJDEBUG(" n is %d, str size %d, lasterror: %x", n, str.size(), GetLastError());
	if (n == 0) {
		return L"Hello world";
	}

	WCHAR* buf = (WCHAR*)malloc(n * sizeof(WCHAR));
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

CRect get_wa_rect()
{
	CRect rect;
	SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0);
	return rect;
}
