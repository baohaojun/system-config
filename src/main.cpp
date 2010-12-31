
#include "snorenotify.h"

#include <QtGui/QApplication>

int main ( int argc, char *argv[] )
{
    QApplication a ( argc, argv );
	SnoreNotify *sn = new SnoreNotify();
    return a.exec();
	delete sn;
}

