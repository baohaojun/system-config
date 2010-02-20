#ifndef LIBGROWL_H
#define LIBGROWL_H

//registers all alert classes in the aray as default alerts
void reister(string classes[]);
//send a notification and return its registration id
int send(string title,string body,string iconPath,int timeout);

#endif // LIBGROWL_H
