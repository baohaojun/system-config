#ifndef __BHJLIB_H__
#define __BHJLIB_H__
#pragma warning( disable : 4786 )
#define BOOST_THREAD_USE_DLL
#include <map>
#include <list>
#include <string>

namespace bhj {
	using namespace std;
	list<string> split(const string& regex, const string& src);
	string remove_pattern(const string& src, const string& pat);
};

#endif
