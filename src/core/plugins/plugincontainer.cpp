/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2014  Patrick von Reth <vonreth@kde.org>


    SnoreNotify is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    SnoreNotify is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with SnoreNotify.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "plugincontainer.h"
#include "../snore.h"
#include "../snore_p.h"
#include "plugins.h"
#include "snorebackend.h"
#include "snorefrontend.h"
#include "../version.h"

#include <QDir>
#include <QDebug>
#include <QMetaEnum>
#include <QApplication>
#include <QSystemSemaphore>

using namespace Snore;

QHash<SnorePlugin::PluginTypes, QHash<QString,PluginContainer*> > PluginContainer::s_pluginCache;


PluginContainer::PluginContainer(QString fileName, QString pluginName, SnorePlugin::PluginTypes type):
    m_pluginFile(fileName),
    m_pluginName(pluginName),
    m_pluginType(type),
    m_loader(pluginDir().absoluteFilePath(file()))
{

}

PluginContainer::~PluginContainer()
{
    m_loader.unload();
}

SnorePlugin *PluginContainer::load()
{
    if(!m_loader.isLoaded() && !m_loader.load())
    {
        snoreDebug( SNORE_WARNING ) << "Failed loading plugin: " << m_loader.errorString();
        return NULL;
    }
    return qobject_cast<SnorePlugin*> ( m_loader.instance());
}

void PluginContainer::unload()
{
    m_loader.unload();
}

const QString & PluginContainer::file()
{
    return m_pluginFile;
}

const QString & PluginContainer::name()
{
    return m_pluginName;
}

SnorePlugin::PluginTypes PluginContainer::type()
{
    return m_pluginType;
}

bool PluginContainer::isLoaded() const
{
    return m_loader.isLoaded();
}

SnorePlugin::PluginTypes PluginContainer::typeFromString(const QString &t)
{
    return (SnorePlugin::PluginTypes)SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType")).keyToValue(t.toUpper().toLatin1());
}

QString PluginContainer::typeToString(const SnorePlugin::PluginTypes t)
{
    return SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType")).valueToKey(t);
}

const QList<SnorePlugin::PluginTypes> &PluginContainer::types()
{
    static QList<SnorePlugin::PluginTypes> t;
    if(t.isEmpty())
    {
        QMetaEnum e = SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType"));
        for (int i = 0; i < e.keyCount(); ++i)
        {
            t << (SnorePlugin::PluginTypes) e.value(i);
        }
    }
    return t;
}

void PluginContainer::updatePluginCache()
{
    snoreDebug( SNORE_DEBUG ) << "Updating plugin cache";
    foreach(PluginContaienrHash list, s_pluginCache)
    {
        foreach(PluginContainer* p, list.values())
        {
            delete p;
        }
        list.clear();
    }
    cache().remove("");

    QList<PluginContainer*> plugins;

    foreach(const SnorePlugin::PluginTypes type,PluginContainer::types())
    {
        foreach(const QFileInfo &file, pluginDir().entryInfoList(
                    QStringList(pluginFileFilters(type)), QDir::Files))
        {
            snoreDebug( SNORE_DEBUG ) << "adding" << file.absoluteFilePath();
            QPluginLoader loader(file.absoluteFilePath());
            QObject *plugin = loader.instance();
            if (plugin == NULL)
            {
                snoreDebug( SNORE_WARNING ) << "Failed loading plugin: " << file.absoluteFilePath() << loader.errorString();
                continue;
            }
            SnorePlugin *sp = qobject_cast<SnorePlugin*>(plugin);
            if(sp == NULL)
            {
                snoreDebug( SNORE_WARNING ) << "Error:" << file.absoluteFilePath() << " is not a Snore plugin" ;
                loader.unload();
                continue;
            }
            PluginContainer *info = new PluginContainer(file.fileName(), sp->name() ,type);
            s_pluginCache[type].insert(info->name(), info);
            plugins << info;
            snoreDebug( SNORE_DEBUG ) << "added" << info->name() << "to cache";
        }
    }
    cache().setValue("version",Version::revision());
    cache().setValue("buildtime",Version::buildTime());
    cache().setValue("pluginPath",pluginDir().absolutePath());

    cache().beginWriteArray("plugins");
    for(int i=0;i< plugins.size();++i)
    {
        cache().setArrayIndex(i);
        cache().setValue("fileName",plugins[i]->file());
        cache().setValue("name", plugins[i]->name());
        cache().setValue("type",(int)plugins[i]->type());
    }
    cache().endArray();
    cache().sync();
}

void PluginContainer::loadPluginCache()
{
    int size = cache().beginReadArray("plugins");
    for(int i=0;i<size;++i)
    {
        cache().setArrayIndex(i);
        SnorePlugin::PluginTypes type = (SnorePlugin::PluginTypes)cache().value("type").toInt();
        QString fileName = cache().value("fileName").toString();
        if(QFile(pluginDir().absoluteFilePath(fileName)).exists())
        {
            PluginContainer *info = new PluginContainer(fileName, cache().value("name").toString(), type);
            s_pluginCache[type].insert(info->name(), info);
        }
        else
        {
            snoreDebug( SNORE_WARNING ) << "Cache Corrupted" << fileName << cache().value("name").toString() << type;
            cache().endArray();
            updatePluginCache();
            return;
        }
    }
    cache().endArray();
}

const QHash<QString, PluginContainer *> PluginContainer::pluginCache(SnorePlugin::PluginTypes type)
{
    if(s_pluginCache.isEmpty())
    {
        if(cache().value("version").toString() != Version::revision() || qgetenv("LIBSNORE_FORCE_CHACHE_UPDATE").toInt() == 1)
        {
            updatePluginCache();
        }
        else
        {
            loadPluginCache();
        }
    }

    QHash<QString, PluginContainer *> out;
    if(type == SnorePlugin::ALL)
    {
        foreach(const SnorePlugin::PluginTypes &t,types())
        {
            out.unite(s_pluginCache.value(t));
        }
    }
    else
    {
        out = s_pluginCache[type];
    }
    return out;
}

const QDir &PluginContainer::pluginDir()
{
    static QDir *path = NULL;
    if(path == NULL)
    {
        QString appDir = qApp->applicationDirPath();
        QStringList list;
#ifdef Q_OS_MAC
        if ( appDir == "MacOS" )
        {
            list << appDir;
            QDir dir(appDir);
            // Development convenience-hack
            dir.cdUp();
            dir.cdUp();
            dir.cdUp();
            appDir = dir.absolutePath();
        }
#endif
        list << appDir
             << QString("%1/../lib/libsnore" SNORE_SUFFIX).arg(appDir)
             << QString("%1/../lib64/libsnore" SNORE_SUFFIX).arg(appDir)
             << QLatin1String(LIBSNORE_PLUGIN_PATH SNORE_SUFFIX);
        foreach(const QString &p, list)
        {
            QDir dir(p);

            if(!dir.entryInfoList(pluginFileFilters()).isEmpty())
            {
                path = new QDir(dir);
                break;
            }
            else
            {
                snoreDebug( SNORE_DEBUG ) << "Possible pluginpath:" << dir.absolutePath() << "does not contain plugins.";
            }
        }
        Q_ASSERT_X(path != NULL, Q_FUNC_INFO, "Failed to find a plugin dir");
        snoreDebug( SNORE_INFO ) << "PluginPath is :" << path->absolutePath();
    }
    return *path;
}


