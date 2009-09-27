#include "bhjlib.h"
#include "stdafx.h"
#include <boost/regex.hpp>

#define open_namespace(n) namespace n {
#define close_namespace(n) }

open_namespace(bhj)
using namespace boost;
using std::list;
using std::string;
lstring_t split(const cstring& regex, const cstring& src)
{
	lstring_t ls;

	boost::regex e(regex);
	boost::sregex_token_iterator i(src.begin(), src.end(), e, -1);
	boost::sregex_token_iterator j;
	while(i != j)
	{
		ls.push_back(string(*i++));
	}

	
	return ls;
}

cstring remove_pattern(const cstring& src, const cstring& pat)
{
	return regex_replace(src, regex(pat), "", match_default | format_perl);
}

cstring::cstring(const CString& CStr) 
{
	reserve(CStr.GetLength());
	for (int i=0; i<CStr.GetLength(); i++) {
		push_back(CStr[i]);
	}
}

cstring::operator CString() 
{
	CString CStr;
	CStr.GetBuffer(size());
	for (string::iterator i = begin(); i != end(); i++) {
		CStr += *i;
	}
	return CStr;
}

cstring::operator wstring()
{
	wchar_t *wbuff = new wchar_t[size()+1];
	const char *str = c_str();
	int n = mbstowcs(wbuff, str, size());
	wbuff[n] = L'\0';
	wstring wstr = wbuff;
	delete []wbuff;
	return wstr;	
}

cstring::operator const char*()
{
	return c_str();
}

bool fields_match(const cstring& src, const cstring& fstr)
{
	lstring_t tokens = split("\\s+", fstr);
	for (lstring_t::iterator i = tokens.begin(); i != tokens.end(); i++) {
		if (!string_contains(src, *i)) {
			return false;
		}
	}
	return true;
}

bool string_contains(const cstring& src, const cstring& tgt)
{
	return src.find(tgt) != std::string::npos;
}

cstring string_format(const char* fmt, ...)
{
	va_list argList;
	va_start(argList, fmt);
	CString str;
	str.FormatV(fmt, argList);
	va_end(argList);
	return str;
}

int FmtMessageBox(const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	CString str;
	str.FormatV(fmt, args);
	va_end(args);
	AfxMessageBox(str);
	return 0;
}

close_namespace(bhj)
