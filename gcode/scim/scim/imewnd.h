#ifndef __IME_WND_H__
#define __IME_WND_H__
#include <windows.h>
#include <atltypes.h>

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
#endif
