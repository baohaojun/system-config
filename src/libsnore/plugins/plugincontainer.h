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

#ifndef PLUGINCONTAINER_H
#define PLUGINCONTAINER_H
#include "libsnore/snore_exports.h"
#include "libsnore/snore_p.h"

#include <QPluginLoader>
#include <QDir>

namespace Snore
{
class PluginContainer;

typedef  QHash<QString, PluginContainer *> PluginContaienrHash;

class PluginContainer
{
public:
    static const QHash<QString, PluginContainer *> pluginCache(SnorePlugin::PluginTypes type);

    PluginContainer(QString fileName, QString pluginName, SnorePlugin::PluginTypes type);
    ~PluginContainer();
    SnorePlugin *load();
    void unload();
    const QString &file();
    const QString &name();
    SnorePlugin::PluginTypes type();

    bool isLoaded() const;

private:
    static QHash<SnorePlugin::PluginTypes, PluginContaienrHash > s_pluginCache;

    void static updatePluginCache();
    static const QDir &pluginDir();
    static inline const QStringList pluginExtentions()
    {
        QStringList out;
#if defined(Q_OS_UNIX)
        out << QStringLiteral("so");
#endif
#if defined(Q_OS_WIN)
        out << QStringLiteral("dll");
#endif
#if defined(Q_OS_MAC)
        out << QStringLiteral("dylib");
#endif
        return out;
    }

    static inline const QStringList pluginFileFilters()
    {
        QStringList out;
        for (const QString &extention : pluginExtentions()) {
            out << QLatin1String("libsnore_*.") + extention;
        }
        return out;
    }

    static inline const QStringList pluginFileFilters(Snore::SnorePlugin::PluginTypes type)
    {
        QStringList out;
        QString typeName = SnorePlugin::typeToString(type).toLower();
        for (const QString &extention : pluginExtentions()) {
            out << QLatin1String("libsnore_") + typeName + QLatin1String("_*.") + extention;
        }
        return out;
    }

    QString m_pluginFile;
    QString m_pluginName;
    SnorePlugin::PluginTypes m_pluginType;
    QPluginLoader m_loader;
    SnorePlugin *m_plugin = nullptr;
};
}

#endif//PLUGINCONTAINER_H
