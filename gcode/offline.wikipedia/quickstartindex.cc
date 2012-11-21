/* quickstartindex.cc: Simplest possible indexer
 *
 * ----START-LICENCE----
 * Copyright 1999,2000,2001 BrightStation PLC
 * Copyright 2003,2004 Olly Betts
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 * -----END-LICENCE-----
 */

#include <xapian.h>
#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <algorithm>

using namespace std;

#define MAX_KEY 230

#define SPLIT_TITLE_INTO_KEYWORDS

// Split a string into its tokens, based on the given delimiters
void Tokenize(const string& str, vector<string>& tokens, const string& delimiters)
{
    // Skip delimiters at beginning.
    string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // Find first "non-delimiter".
    string::size_type pos     = str.find_first_of(delimiters, lastPos);

    while (string::npos != pos || string::npos != lastPos)
    {
        // Found a token, add it to the vector.
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        // Skip delimiters.  Note the "not_of"
        lastPos = str.find_first_not_of(delimiters, pos);
        // Find next "non-delimiter"
        pos = str.find_first_of(delimiters, lastPos);
    }
}

// We will perform case insensitive searches, so we need a function to lowcase() a string
char to_lower (const char c) { return tolower(c); } 
void lowcase(string& s) { transform(s.begin(), s.end(), s.begin(), to_lower); }

int main(int argc, char **argv)
{
    // Simplest possible options parsing: we just require three or more
    // parameters.

    // Catch any Xapian::Error exceptions thrown
    
    unsigned total = 0;

    try {
        // Make the database
        Xapian::WritableDatabase database("db/", Xapian::DB_CREATE_OR_OPEN);

	string docId;
        string block_addr;
        string prev_block_addr;
	while(1) {
	    string title;
	    if (cin.eof()) break;
	    getline(cin, title);
	    int l = title.length();
            if (l < 6)
              continue;

	    if (title.substr(0, 6) == "block:") {
              prev_block_addr = block_addr;
              block_addr = title.substr(7, string::npos);
              if (prev_block_addr.empty()) {
                prev_block_addr = block_addr;
              }
              // cout << "block_addr is " << block_addr << endl;
              continue;
	    }

            unsigned int title_end_pos = title.find('\t');
	    string Title = title.substr(7, title_end_pos - 7);

            string title_addr = title.substr(title.find('\t') + 1);
            // cout << "title_addr is " << title_addr << endl;
            title = Title;

	    // Make the document
	    Xapian::Document newdocument;

	    // Target: filename and the exact title used
	    string target = title + "\t" + prev_block_addr + " " + block_addr + " " + title_addr;
            //cout << "target is " << target << endl;
	    newdocument.set_data(target);

	    // 1st Source: the title
	    if (title.length() > MAX_KEY)
		title = title.substr(0, MAX_KEY);
	    newdocument.add_posting(title.c_str(), 1);

	    lowcase(title);

	    vector<string> keywords;
            // cout << "title is " << title << endl;
	    Tokenize(title, keywords, " ");
            
	    // 2nd source: All the title's lowercased words
	    int cnt = 2;
	    for (vector<string>::iterator it=keywords.begin(); it!=keywords.end(); it++) {
		if (it->length() > MAX_KEY)
		    *it = it->substr(0, MAX_KEY);
		newdocument.add_posting(it->c_str(), cnt++);
	    }

	    try {
		// cout << "Added " << title << endl;
		// Add the document to the database
		database.add_document(newdocument);
	    } catch(const Xapian::Error &error) {
		cout << "Exception: "  << error.get_msg();
		cout << "\nWhen adding:\n" << title;
		cout << "\nOf length " << title.length() << endl;
	    }
	    total ++;

	    if ((total % 8192) == 0) {
		cout << total << " articles indexed so far" << endl;
	    }
	}
    } catch(const Xapian::Error &error) {
        cout << "Exception: "  << error.get_msg() << endl;
    }
    cout << total << " articles indexed." << endl;
}

