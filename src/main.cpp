
#include "core/snoreserver.h"
#include "trayicon.h"

#include <QtGui/QApplication>
#include <QDir>
#include <QFile>
#include <QList>
#include <QDebug>
#include <QPluginLoader>
#include <QSystemTrayIcon>

int main ( int argc, char *argv[] )
{
    QApplication a ( argc, argv );
    QSystemTrayIcon *trayIcon=new QSystemTrayIcon(QIcon(":/root/zzz.png"));
    trayIcon->setVisible(true);

    SnoreServer *s = new SnoreServer( trayIcon );
    s->cleanupTMP();

    QDir pluginsDir ( a.applicationDirPath() +"/snoreplugins" );
    foreach ( QString fileName, pluginsDir.entryList ( QDir::Files ) )
    {
        s->publicatePlugin ( pluginsDir.absoluteFilePath ( fileName ) );
    }

    TrayIcon *i = new TrayIcon(trayIcon,s);
    i->initConextMenu();

    return a.exec();
}

