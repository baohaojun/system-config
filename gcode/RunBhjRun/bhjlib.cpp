#include "bhjlib.h"
#include <boost/regex.hpp>

#define open_namespace(n) namespace n {
#define close_namespace(n) }

open_namespace(bhj)
using namespace std;
using namespace boost;
list<string> split(const string& regex, const string& src)
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

string remove_pattern(const string& src, const string& pat)
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
close_namespace(bhj)
