#include <QtGui/QApplication>
#include "core/snoreserver.h"
#include <QDir>
#include <QFile>
#include <QList>
#include <QDebug>
#include <QPluginLoader>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    SnoreServer s;

    QDir pluginsDir(a.applicationDirPath()+"/snoreplugins");
    foreach (QString fileName, pluginsDir.entryList(QDir::Files)) {
            s.publicatePlugin(pluginsDir.absoluteFilePath(fileName));
    }



    return a.exec();
}

