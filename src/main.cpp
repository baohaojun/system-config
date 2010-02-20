#include <QtCore/QCoreApplication>
#include "core/snoreserver.h"
#include <QDir>
#include <QFile>
#include <QList>
#include <QDebug>
#include <QPluginLoader>

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    SnoreServer s;

    QDir pluginsDir(a.applicationDirPath()+"/plugins");
    foreach (QString fileName, pluginsDir.entryList(QDir::Files)) {
        QPluginLoader loader(pluginsDir.absoluteFilePath(fileName));
        QObject *plugin = loader.instance();        
        if (plugin) {
            s.publicatePlugin(plugin);
        }else
            qDebug()<<"Failed loading plugin: "<<loader.errorString();
    }



    return a.exec();
}

