/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2015  Hannah von Reth <vonreth@kde.org>

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
#include <QGuiApplication>
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
        qCWarning(SNORE) << "Failed loading plugin: " << m_loader.errorString();
        return nullptr;
    }
    if (!m_plugin) {
        m_plugin = qobject_cast<SnorePlugin *> (m_loader.instance());
        m_plugin->m_container = this;
        m_plugin->setDefaultSettings();
    }
    return m_plugin;
}

void PluginContainer::unload()
{
    m_loader.unload();
    m_plugin = nullptr;
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

void PluginContainer::updatePluginCache()
{
    qCDebug(SNORE) << "Updating plugin cache";
    foreach(PluginContaienrHash list, s_pluginCache.values()) {
        qDeleteAll(list);
        list.clear();
    }

    foreach(const SnorePlugin::PluginTypes type, SnorePlugin::types()) {
        foreach(const QFileInfo & file, pluginDir().entryInfoList(
                    QStringList(pluginFileFilters(type)), QDir::Files)) {
            qCDebug(SNORE) << "adding" << file.absoluteFilePath();
            QPluginLoader loader(file.absoluteFilePath());
            QJsonObject data = loader.metaData()[QStringLiteral("MetaData")].toObject();
            QString name = data.value(QStringLiteral("name")).toString();
            if (!name.isEmpty()) {
                PluginContainer *info = new PluginContainer(file.fileName(), name, type);
                s_pluginCache[type].insert(name, info);
                qCDebug(SNORE) << "added" << type << ":" << name << "to cache";
            }
        }
    }
}

const QHash<QString, PluginContainer *> PluginContainer::pluginCache(SnorePlugin::PluginTypes type)
{
    if (s_pluginCache.isEmpty()) {
        QTime time;
        time.start();
        updatePluginCache();
        qCDebug(SNORE) << "Plugins loaded in:" << time.elapsed();
    }

    QHash<QString, PluginContainer *> out;
    for (auto &t : SnorePlugin::types()) {
        if (t & type) {
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
        if (appDir == QLatin1String("MacOS")) {
            list << appDir;
            QDir dir(appDir);
            // Development convenience-hack
            dir.cdUp();
            dir.cdUp();
            dir.cdUp();
            appDir = dir.absolutePath();
        }
#endif
        QString suffix = QLatin1String("/libsnore") + QLatin1String(SNORE_SUFFIX);
        list << appDir
             << QStringLiteral(LIBSNORE_PLUGIN_PATH)
             << appDir + suffix
             << appDir + QStringLiteral("/../lib/plugins") + suffix
             << appDir + QStringLiteral("/../lib64/plugins") + suffix;
        foreach(const QString & p, list) {
            path = QDir(p);

            if (!path.entryInfoList(pluginFileFilters()).isEmpty()) {
                break;
            } else {
                qCDebug(SNORE) << "Possible pluginpath:" << path.absolutePath() << "does not contain plugins.";
            }
        }
        if (path.entryInfoList(pluginFileFilters()).isEmpty()) {
            qCWarning(SNORE) << "Couldnt find any plugins";
        }
        qCDebug(SNORE) << "PluginPath is :" << path.absolutePath();
    }
    return path;
}

