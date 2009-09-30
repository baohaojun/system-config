#include "stdafx.h"
#include "bhjlib.h"
#include "stdafx.h"
#include <boost/regex.hpp>
#define ENABLE_BHJDEBUG
#include "bhjdebug.h" 

#define OPEN_NAMESPACE(n) namespace n {
#define CLOSE_NAMESPACE(n) }

OPEN_NAMESPACE(bhj)
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

cstring dirname(const cstring& path)
{
	cstring p = regex_replace(path, regex("\\\\+"), "/", match_default|format_perl);
	while (p[p.size() -1] == '/') {
		p.erase(p.size() - 1);
	}

	if (!string_contains(p, "/")) {
		return ".";
	}

	int n = p.find_last_of("/");
	p.erase(n);
	return p;
}

cstring bce_dirname(const cstring& path)
{
	cstring p = regex_replace(path, regex("\\\\+"), "/", match_default|format_perl);

	if (!string_contains(p, "/")) {
		if (p.size()==2 && p.at(1)==':') {
			return p + "/";
		}
		return ".";
	}

	int n = p.find_last_of("/");
	p.erase(n);
	if (p.size()==2 && p.at(1)==':') {
		return p + "/";
	}
	return p;

}

cstring basename(const cstring& path)
{
	cstring p = regex_replace(path, regex("\\\\+"), "/", match_default|format_perl);
	while (p[p.size() -1] == '/') {
		p.erase(p.size() - 1);
	}

	if (!string_contains(p, "/")) {
		return p;
	}

	int n = p.find_last_of("/");
	p.erase(0, n);
	return p.empty() ? "/" : p;

}

cstring bce_basename(const cstring& path)
{
	cstring p = regex_replace(path, regex("\\\\+"), "/", match_default|format_perl);

	if (p[p.size() - 1] == '/') {
		return "";
	}

	if (!string_contains(p, "/")) { //if it is like `c:', return `c:/'
		if (p.size()>1 && p.at(1)==':') {
			return "";
		}
		return p;
	}

	int n = p.find_last_of("/");
	p.erase(0, n+1);
	return p;

}

bool is_abspath(const cstring& path)
{
	if (path.size() < 2) {
		return false;
	}
	return (path[1] == ':');
}

lstring_t getMatchingFiles(const cstring& dir, const cstring& base)
{
	lstring_t ls_match;
	WIN32_FIND_DATA wfd;
	BHJDEBUG(" patten is %s", (dir+"/*"+base+"*").c_str());
	HANDLE hfile = FindFirstFile(cstring(dir + "/*" + base + "*"), &wfd);
	while (hfile != INVALID_HANDLE_VALUE) {
		if (dir.at(dir.size() - 1) == '/') {
			ls_match.push_back(dir + wfd.cFileName);
		} else {
			ls_match.push_back(dir + "/" + wfd.cFileName);
		}
		if (FindNextFile(hfile, &wfd) == 0) {
			break;
		}
	}
	return ls_match;
}

void debug_lstring(const lstring_t& ls)
{
	for (lstring_t::const_iterator i = ls.begin(); i != ls.end(); i++) {
		BHJDEBUG(" %s", i->c_str());
	}
}
CLOSE_NAMESPACE(bhj)
