#ifndef _IME_SOCKET_H__
#define _IME_SOCKET_H__
#include <winsock.h>
#include <string>
using std::string;

void connect_ime_server();
bool init_ime_socket();
string sock_error();
extern SOCKET g_ime_sock;

#endif //_IME_SOCKET_H__
