#include <QtGui/QApplication>
#include "core/snoreserver.h"
#include <QDir>
#include <QFile>
#include <QList>
#include <QDebug>
#include <QPluginLoader>
#include <QSystemTrayIcon>

int main ( int argc, char *argv[] )
{
    QApplication a ( argc, argv );
    QSystemTrayIcon *trayIcon=new QSystemTrayIcon();
    trayIcon->show();
    SnoreServer s ( trayIcon );

    QDir pluginsDir ( a.applicationDirPath() +"/snoreplugins" );
    foreach ( QString fileName, pluginsDir.entryList ( QDir::Files ) )
    {
        s.publicatePlugin ( pluginsDir.absoluteFilePath ( fileName ) );
    }



    return a.exec();
}

