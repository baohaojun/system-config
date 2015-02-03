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
#include <QTime>

using namespace Snore;

QHash<SnorePlugin::PluginTypes, QHash<QString, PluginContainer *> > PluginContainer::s_pluginCache;

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
    if (!m_loader.isLoaded() && !m_loader.load()) {
        snoreDebug(SNORE_WARNING) << "Failed loading plugin: " << m_loader.errorString();
        return NULL;
    }
    SnorePlugin *plugin = qobject_cast<SnorePlugin *> (m_loader.instance());
    Q_ASSERT_X(m_pluginName == plugin->name(),Q_FUNC_INFO, "The plugin name is different to the one in the meta data.");
    plugin->m_type = type();
    return plugin;
}

void PluginContainer::unload()
{
    m_loader.unload();
}

const QString &PluginContainer::file()
{
    return m_pluginFile;
}

const QString &PluginContainer::name()
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
    if (t.isEmpty()) {
        QMetaEnum e = SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType"));
        for (int i = 0; i < e.keyCount(); ++i) {
            t << (SnorePlugin::PluginTypes) e.value(i);
        }
    }
    return t;
}

void PluginContainer::updatePluginCache()
{
    snoreDebug(SNORE_DEBUG) << "Updating plugin cache";
    foreach(PluginContaienrHash list, s_pluginCache) {
        foreach(PluginContainer * p, list.values()) {
            delete p;
        }
        list.clear();
    }

    foreach(const SnorePlugin::PluginTypes type, PluginContainer::types()) {
        foreach(const QFileInfo & file, pluginDir().entryInfoList(
                    QStringList(pluginFileFilters(type)), QDir::Files)) {
            snoreDebug(SNORE_DEBUG) << "adding" << file.absoluteFilePath();
            QPluginLoader loader(file.absoluteFilePath());
            QJsonObject data = loader.metaData()["MetaData"].toObject();
            QString name = data.value("name").toString();
            if (!name.isEmpty()) {
                PluginContainer *info = new PluginContainer(file.fileName(), name, type);
                s_pluginCache[type].insert(name, info);
                snoreDebug(SNORE_DEBUG) << "added" << name << "to cache";
            }
        }
    }
}

const QHash<QString, PluginContainer *> PluginContainer::pluginCache(SnorePlugin::PluginTypes type)
{
    snoreDebug(SNORE_DEBUG) << type;
    if (s_pluginCache.isEmpty()) {
        QTime time;
        time.start();
        updatePluginCache();
        snoreDebug(SNORE_DEBUG) << "Plugins loaded in:" << time.elapsed();
    }

    QHash<QString, PluginContainer *> out;
    for(auto t:types()){
        if(t & type)
        {
            out.unite(s_pluginCache.value(t));
        }
    }
    return out;
}

const QDir &PluginContainer::pluginDir()
{
    static bool isLoaded = false;
    static QDir path;
    if (!isLoaded) {
        isLoaded = true;
        QString appDir = qApp->applicationDirPath();
        QStringList list;
#ifdef Q_OS_MAC
        if (appDir == "MacOS") {
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
             << QLatin1String(LIBSNORE_PLUGIN_PATH)
             << QString("%1/libsnore" SNORE_SUFFIX).arg(appDir)
             << QString("%1/../lib/plugins/libsnore" SNORE_SUFFIX).arg(appDir)
             << QString("%1/../lib64/plugins/libsnore" SNORE_SUFFIX).arg(appDir);
        foreach(const QString & p, list) {
            path = QDir(p);

            if (!path.entryInfoList(pluginFileFilters()).isEmpty()) {
                break;
            } else {
                snoreDebug(SNORE_DEBUG) << "Possible pluginpath:" << path.absolutePath() << "does not contain plugins.";
            }
        }
        if (path.entryInfoList(pluginFileFilters()).isEmpty()) {
            snoreDebug(SNORE_WARNING) << "Couldnt find any plugins";
        }
        snoreDebug(SNORE_INFO) << "PluginPath is :" << path.absolutePath();
    }
    return path;
}

