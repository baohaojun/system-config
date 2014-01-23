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
    if ( !m_loader.load())
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

SnorePlugin::PluginTypes PluginContainer::typeFromString(const QString &t)
{
    return (SnorePlugin::PluginTypes)SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType")).keyToValue(t.toUpper().toLatin1());
}

const QStringList &PluginContainer::types()
{
    static QStringList out;
    if(out.isEmpty())
    {
        QMetaEnum e = SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType"));
        for(int i=0;i<e.keyCount();++i)
        {
            out << QString::fromLatin1(e.key(i));
        }
    }
    return out;
}


void PluginContainer::updatePluginCache()
{
    snoreDebug( SNORE_DEBUG ) << "Updating plugin cache";
    cache().remove("");

#if defined(Q_OS_LINUX)
    const QString extensions = "so";
#elif defined(Q_OS_WIN)
    const QString extensions = "dll";
#elif defined(Q_OS_MAC)
    const QString extensions = "dylib";
#endif

    foreach(const QString &type,PluginContainer::types())
    {
        foreach (const QFileInfo &file, pluginDir().entryInfoList(QStringList(QString("libsnore_%1_*.%2").arg(type.toLower(), extensions)), QDir::Files, QDir::Name | QDir::IgnoreCase ))
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
            PluginContainer *info = new PluginContainer(file.fileName(), sp->name() ,PluginContainer::typeFromString(type));
            s_pluginCache[info->type()].insert(info->name(),info);
            snoreDebug( SNORE_DEBUG ) << "added" << info->name() << "to cache";
        }
    }
    cache().setValue("version",Version::revision());
    cache().setValue("buildtime",Version::buildTime());
    cache().setValue("pluginPath",pluginDir().absolutePath());
    QList<PluginContainer*> plugins = pluginCache(SnorePlugin::ALL).values();
    cache().beginWriteArray("plugins");
    for(int i=0;i< plugins.size();++i)
    {
        cache().setArrayIndex(i);
        cache().setValue("fileName",plugins[i]->file());
        cache().setValue("name", plugins[i]->name());
        cache().setValue("type",(int)plugins[i]->type());
    }
    cache().endArray();
}

const QHash<QString, PluginContainer *> PluginContainer::pluginCache(SnorePlugin::PluginTypes type)
{
    if(s_pluginCache.isEmpty())
    {
        QString version = cache().value("version").toString();
        QString buildTime = cache().value("buildtime").toString();
        int size = cache().beginReadArray("plugins");
        if(size == 0 ||
                version != Version::revision() ||
                buildTime != Version::buildTime())
        {
            cache().endArray();
            updatePluginCache();
        }
        else
        {
            for(int i=0;i<size;++i)
            {
                cache().setArrayIndex(i);
                SnorePlugin::PluginTypes type = (SnorePlugin::PluginTypes)cache().value("type").toInt();
                PluginContainer *info = new PluginContainer(cache().value("fileName").toString(),cache().value("name").toString(),type);
                s_pluginCache[type].insert(info->name(), info);
            }
            cache().endArray();
        }
    }

    QHash<QString, PluginContainer *> out;
    if(type == SnorePlugin::ALL)
    {
        QMetaEnum e = SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType"));
        for (int i = 0; i < e.keyCount(); ++i)
        {
            out.unite(s_pluginCache.value((SnorePlugin::PluginTypes) e.value(i)));
        }
    }
    else
    {
        out = s_pluginCache[type];
    }
    return out;
}

const QDir PluginContainer::pluginDir()
{
    static QString path;
    if(path.isNull())
    {
        const QString appDir = qApp->applicationDirPath();
        QStringList list;
        list << QString("%1/../lib/libsnore").arg(appDir)
             << QString("%1/../lib64/libsnore").arg(appDir)
             << QString("%1/libsnore").arg(appDir)
             << QLatin1String(LIBSNORE_PLUGIN_PATH);
        foreach(const QString &p, list)
        {
            QDir dir(p);
            if(dir.exists())
            {
                path = dir.absolutePath();
                break;
            }
            else
            {
                snoreDebug( SNORE_DEBUG ) << "Possible pluginpath:" << dir.absolutePath() << "does not exist";
            }
        }
        snoreDebug( SNORE_INFO ) << "PluginPath is :" << path;
    }
    return QDir(path);
}


