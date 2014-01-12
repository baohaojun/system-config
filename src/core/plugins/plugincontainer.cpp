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
        qDebug() << "Failed loading plugin: " << m_loader.errorString();
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
    qDebug() << "Updating plugin cache";

    s_pluginCache.clear();
    clear();

    foreach(const QString &type,PluginContainer::types()){
        QDir plPath(SnoreCorePrivate::pluginDir().absoluteFilePath(type));
        qDebug() << "Searching for plugins in" << plPath.path();
        foreach (QString fileName, plPath.entryList(QDir::Files))
        {
            QString filepath(plPath.absoluteFilePath(fileName));
            qDebug() << "adding" << filepath;
            QPluginLoader loader(filepath);
            QObject *plugin = loader.instance();
            if (plugin == NULL) {
                qDebug() << "Failed loading plugin: " << filepath << loader.errorString();
                continue;
            }
            SnorePlugin *sp = qobject_cast<SnorePlugin*>(plugin);
            if(sp == NULL){
                qDebug() << "Error:" << fileName << " is not a Snore plugin" ;
                plugin->deleteLater();
                continue;
            }
            PluginContainer *info = new PluginContainer( SnoreCorePrivate::pluginDir().relativeFilePath(filepath),sp->name(),PluginContainer::typeFromString(type));
            s_pluginCache.insert(info->name(),info);
            qDebug() << "added" << info->name() << "to cache";
        }
    }

    qDebug()<<s_pluginCache.keys();
    setValue("version",Version::revision());
    setValue("buildtime",Version::buildTime());
    setValue("pluginPath",SnoreCorePrivate::pluginDir().absolutePath());
    QList<PluginContainer*> plugins = s_pluginCache.values();
    beginWriteArray("plugins");
    for(int i=0;i< plugins.size();++i) {
        setArrayIndex(i);
        setValue("fileName",plugins[i]->file());
        setValue("name", plugins[i]->name());
        setValue("type",(int)plugins[i]->type());
    }
    endArray();
}

QHash<QString, PluginContainer *> PluginContainer::pluginCache(){
    if(!s_pluginCache.isEmpty())
        return s_pluginCache;
    QString version = value("version").toString();
    QString buildTime = value("buildtime").toString();
    int size = beginReadArray("plugins");
    if(size == 0 ||
            version != Version::revision() ||
            buildTime != Version::buildTime())
    {
        endArray();
        updatePluginCache();
    }
    else
    {
        for(int i=0;i<size;++i)
        {
            setArrayIndex(i);
            SnorePlugin::PluginType type = (SnorePlugin::PluginType)value("type").toInt();
            PluginContainer *info = new PluginContainer(value("fileName").toString(),value("name").toString(),type);
            s_pluginCache.insert(info->name(),info);
        }
        endArray();
    }

    return s_pluginCache;
}
