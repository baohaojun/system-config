#ifndef __IME_WND_H__
#define __IME_WND_H__
#include <windows.h>
#include <atltypes.h>
#include <immdev.h>
#include <string>
#include <map>
#include <vector>
using namespace std;

typedef UINT u32;
class CBhjWnd {
public:
	CBhjWnd() {m_hWnd = 0;};
	virtual void paint() = 0;
	virtual void init(HWND hwnd) {
		m_hWnd = hwnd;
	};
private:
	HWND m_hWnd;
};

class CBhjStatusWnd : public CBhjWnd 
{
public:
	virtual void paint() {};
};

class CBhjCompCandWnd : public CBhjWnd
{
public:
	virtual void paint() {};
};

void debug_rect(const CRect& rect);
void FillSolidRect(HDC hdc, const CRect& rect, COLORREF rgb);

class input_context
{
public:	
	HIMC get_handle() {
		return m_himc;
	}
	input_context(HIMC himc, LPTRANSMSG msg_buf, u32 msg_buf_size = 0);
	input_context(HWND hwnd);
	~input_context() {
		if (m_himc) {
			ImmUnlockIMC(m_himc);
		}
	}

	int send_text(const string&);
	
	LPINPUTCONTEXT operator->() {
		return m_ic;
	}
	
	operator bool() {
		if (m_ic) {
			return true;
		} else {
			return false;
		}
	}

	bool add_msg(u32 msg, WPARAM wp = 0, LPARAM lp = 0);
	u32 return_ime_msgs();
	bool add_show_comp_msg();
	bool add_update_status_msg();
private:
	bool enlarge_msg_buf(u32 n);
	bool copy_old_msg();

private:
	//to prevent copying..
	input_context(const input_context&);
	input_context& operator=(const input_context&); 

private:
	LPINPUTCONTEXT m_ic;
	HIMC m_himc;
	const LPTRANSMSG m_msg_buf;
	const u32 m_msg_buf_size;
	u32 m_num_msg;
};

class hdc_with_font
{
public:
	hdc_with_font(HDC, wstring);
	~hdc_with_font();
	void use_this_font();
	void draw_text(const wstring& text, CRect& rect);
	int get_text_width(const wstring& text);
private:
	HFONT m_old_font, m_this_font;
	HDC m_dc;
};

extern string g_comp_str;

// int MultiByteToWideChar(
//   UINT CodePage, 
//   DWORD dwFlags,         
//   LPCSTR lpMultiByteStr, 
//   int cbMultiByte,       
//   LPWSTR lpWideCharStr,  
//   int cchWideChar        
// );

wstring to_wstring(const string& str);
string to_string(const wstring& wstr);
CRect get_wa_rect();
const int comp_dft_width = 600;
const int comp_dft_height = 60;
typedef map<string, vector<string>> rule_map_t;
extern rule_map_t g_quail_rules;
extern u32 g_first_cand, g_last_cand, g_active_cand;
extern string g_ime_name;
extern const char *const ime_off;
extern const char *const ime_on;
extern "C" char *strcasestr(const char *S, const char *FIND);
#endif
