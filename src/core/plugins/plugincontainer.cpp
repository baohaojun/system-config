/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013  Patrick von Reth <vonreth@kde.org>


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


#include "../snore.h"
#include "plugins.h"
#include "snorebackend.h"
#include "snorefrontend.h"

#include <QDir>
#include <QDebug>

using namespace Snore;

PluginContainer::PluginContainer(QString fileName, QString pluginName, PluginContainer::PluginType type):
    m_pluginFile(fileName),
    m_pluginName(pluginName),
    m_pluginType(type)
{
}

PluginContainer::~PluginContainer()
{
    if(m_instance)
    {
        m_instance->deleteLater();
    }
}

SnorePlugin *PluginContainer::load()
{
    if(m_instance != NULL)
        return m_instance;
    QPluginLoader loader ( SnoreCore::pluginDir().absoluteFilePath(file()));
    qDebug() << "Trying to load" << file();
    if ( !loader.load())
    {
        qDebug() << "Failed loading plugin: " << loader.errorString();
        return NULL;
    }

    m_instance = qobject_cast<SnorePlugin*> ( loader.instance());
    return m_instance;
}

const QString & PluginContainer::file()
{
    return m_pluginFile;
}

const QString & PluginContainer::name()
{
    return m_pluginName;
}

PluginContainer::PluginType PluginContainer::type()
{
    return m_pluginType;
}

PluginContainer::PluginType PluginContainer::typeFromString(const QString &t)
{
    PluginContainer::PluginType type = PLUGIN;
    if(t == QLatin1String("backend"))
    {
        type = BACKEND;
    }
    else if(t == QLatin1String("secondary_backend"))
    {
        type = SECONDARY_BACKEND;
    }
    else if(t == QLatin1String("frontend"))
    {
        type = FRONTEND;
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
