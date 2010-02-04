#ifndef _IME_SOCKET_H__
#define _IME_SOCKET_H__
#include <string>
using std::string;
void connect_ime_server();
string sock_error();

string ime_recv_line();
void ime_write_line(const string& line);

#endif //_IME_SOCKET_H__
