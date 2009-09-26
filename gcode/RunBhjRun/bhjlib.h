#ifndef __BHJLIB_H__
#define __BHJLIB_H__
#pragma warning( disable : 4786 )
#define BOOST_THREAD_USE_DLL
#include <map>
#include <list>
#include <string>

namespace bhj {
	using namespace std;


	class cstring : public string {
	public:
		cstring(const CString&);
		cstring(const string& str) : string(str) {};
		cstring(const char* c_str) : string(c_str) {};
		operator CString();
	};


	list<cstring> split(const cstring& regex, const cstring& src);
	cstring remove_pattern(const cstring& src, const cstring& pat);
};

#endif
