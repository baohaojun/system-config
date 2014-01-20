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

using namespace Snore;

QHash<QString,PluginContainer*> PluginContainer::s_pluginCache = QHash<QString,PluginContainer*>() ;


PluginContainer::PluginContainer(QString fileName, QString pluginName, SnorePlugin::PluginType type):
    m_pluginFile(fileName),
    m_pluginName(pluginName),
    m_pluginType(type),
    m_loader(SnoreCorePrivate::pluginDir().absoluteFilePath(file()))
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
        snoreDebug( SNORE_DEBUG ) << "Failed loading plugin: " << m_loader.errorString();
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

SnorePlugin::PluginType PluginContainer::type()
{
    return m_pluginType;
}

SnorePlugin::PluginType PluginContainer::typeFromString(const QString &t)
{
    SnorePlugin::PluginType type = SnorePlugin::PLUGIN;
    if(t == QLatin1String("backend"))
    {
        type = SnorePlugin::BACKEND;
    }
    else if(t == QLatin1String("secondary_backend"))
    {
        type = SnorePlugin::SECONDARY_BACKEND;
    }
    else if(t == QLatin1String("frontend"))
    {
        type = SnorePlugin::FRONTEND;
    }
    return type;
}

const QStringList &PluginContainer::types()
{
    static QStringList list;
    if(list.isEmpty()){
        list << "backend"
             << "secondary_backend"
             << "frontend"
             << "plugin";
    }
    return list;
}


void PluginContainer::updatePluginCache(){
    snoreDebug( SNORE_DEBUG ) << "Updating plugin cache";

    s_pluginCache.clear();
    cache().remove("");

    foreach(const QString &type,PluginContainer::types()){
        QDir plPath(SnoreCorePrivate::pluginDir().absoluteFilePath(type));
        snoreDebug( SNORE_DEBUG ) << "Searching for plugins in" << plPath.path();
        foreach (QString fileName, plPath.entryList(QDir::Files))
        {
            QString filepath(plPath.absoluteFilePath(fileName));
            snoreDebug( SNORE_DEBUG ) << "adding" << filepath;
            QPluginLoader loader(filepath);
            QObject *plugin = loader.instance();
            if (plugin == NULL) {
                snoreDebug( SNORE_DEBUG ) << "Failed loading plugin: " << filepath << loader.errorString();
                continue;
            }
            SnorePlugin *sp = qobject_cast<SnorePlugin*>(plugin);
            if(sp == NULL){
                snoreDebug( SNORE_DEBUG ) << "Error:" << fileName << " is not a Snore plugin" ;
                loader.unload();
                continue;
            }
            PluginContainer *info = new PluginContainer( SnoreCorePrivate::pluginDir().relativeFilePath(filepath),sp->name(),PluginContainer::typeFromString(type));
            s_pluginCache.insert(info->name(),info);
            snoreDebug( SNORE_DEBUG ) << "added" << info->name() << "to cache";
        }
    }
    cache().setValue("version",Version::revision());
    cache().setValue("buildtime",Version::buildTime());
    cache().setValue("pluginPath",SnoreCorePrivate::pluginDir().absolutePath());
    QList<PluginContainer*> plugins = s_pluginCache.values();
    cache().beginWriteArray("plugins");
    for(int i=0;i< plugins.size();++i) {
        cache().setArrayIndex(i);
        cache().setValue("fileName",plugins[i]->file());
        cache().setValue("name", plugins[i]->name());
        cache().setValue("type",(int)plugins[i]->type());
    }
    cache().endArray();
}

QHash<QString, PluginContainer *> PluginContainer::pluginCache(){
    if(!s_pluginCache.isEmpty())
    {
        return s_pluginCache;
    }
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
            SnorePlugin::PluginType type = (SnorePlugin::PluginType)cache().value("type").toInt();
            PluginContainer *info = new PluginContainer(cache().value("fileName").toString(),cache().value("name").toString(),type);
            s_pluginCache.insert(info->name(),info);
        }
        cache().endArray();
    }

    return s_pluginCache;
}
