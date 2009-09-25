#ifndef __BHJLIB_H__
#define __BHJLIB_H__

#include <map>
#include <list>
#include <string>
using std::map;
using std::list;
using std::string;

namespace bhj {
	list<string> split(const string& regex, const string& src);
	string join(const string sep, const list<string>& ls);
};

#endif
