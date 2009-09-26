#include "bhjlib.h"
#include "stdafx.h"
#include <boost/regex.hpp>

#define open_namespace(n) namespace n {
#define close_namespace(n) }

open_namespace(bhj)
using namespace boost;
using std::list;
using std::string;
list<string> split(const cstring& regex, const cstring& src)
{
	list<string> ls;

	boost::regex e(regex);
	boost::sregex_token_iterator i(src.begin(), src.end(), e, -1);
	boost::sregex_token_iterator j;
	while(i != j)
	{
		ls.push_back(*i++);
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
close_namespace(bhj)
