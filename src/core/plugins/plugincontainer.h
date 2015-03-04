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

#ifndef PLUGINCONTAINER_H
#define PLUGINCONTAINER_H
#include "snore_exports.h"
#include "../snore_p.h"

#include <QPluginLoader>
#include <QDir>

namespace Snore
{
class PluginContainer;

typedef  QHash<QString, PluginContainer *> PluginContaienrHash;

class SNORE_EXPORT PluginContainer
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
        out << QLatin1String("so");
#endif
#if defined(Q_OS_WIN)
        out << QLatin1String("dll");
#endif
#if defined(Q_OS_MAC)
        out << QLatin1String("dylib");
#endif
        return out;
    }

    static inline const QStringList pluginFileFilters()
    {
        QStringList out;
        foreach(const QString extention,  pluginExtentions()) {
            out << QString("libsnore_*.%1").arg(extention);
        }
        return out;
    }

    static inline const QStringList pluginFileFilters(Snore::SnorePlugin::PluginTypes type)
    {
        QStringList out;
        foreach(const QString extention,  pluginExtentions()) {
            out << QString("libsnore_%1_*.%2").arg(SnorePlugin::typeToString(type).toLower(), extention);
        }
        return out;
    }

    QString m_pluginFile;
    QString m_pluginName;
    SnorePlugin::PluginTypes m_pluginType;
    QPluginLoader m_loader;
};
}

#endif//PLUGINCONTAINER_H
