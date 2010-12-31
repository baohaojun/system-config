
#include "snorenotify.h"

#include <QtGui/QApplication>

int main ( int argc, char *argv[] )
{
    QApplication a ( argc, argv );
	SnoreNotify();
    return a.exec();
}

