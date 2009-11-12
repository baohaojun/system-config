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
