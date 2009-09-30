#ifndef __BHJLIB_H__
#define __BHJLIB_H__
#pragma warning( disable : 4786 )
#define BOOST_THREAD_USE_DLL
#include <map>
#include <list>
#include <string>


namespace bhj {
	using std::list;
	using std::string;
	using std::wstring;


	class cstring : public string {
	public:
		cstring() {};
		cstring(const CString&);
		cstring(const string& str) : string(str) {};
		cstring(const char* c_str) : string(c_str) {};
		operator CString();
		operator wstring();
		operator const char*();
	};

	typedef list<cstring> lstring_t;
	list<cstring> split(const cstring& regex, const cstring& src);
	cstring remove_pattern(const cstring& src, const cstring& pat);
	bool fields_match(const cstring& src, const cstring& fstr);

	bool string_contains(const cstring& src, const cstring& tgt);
	cstring string_format(const char* fmt, ...);
	int FmtMessageBox(const char* fmt, ...);
	cstring dirname(const cstring& path);
	cstring basename(const cstring& path);

	cstring bce_dirname(const cstring& path); //bce: basename can be empty, take everything before the last '/'.
	cstring bce_basename(const cstring& path);
	bool is_abspath(const cstring& path);

	lstring_t getMatchingFiles(const cstring& dir, const cstring& base);
	void debug_lstring(const lstring_t& ls);
};

#endif

