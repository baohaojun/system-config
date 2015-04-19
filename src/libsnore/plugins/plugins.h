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

#ifndef SNORE_PLUGINS_H
#define SNORE_PLUGINS_H
#include "libsnore/snore_exports.h"
#include "libsnore/snoreglobals.h"
#include "libsnore/notification/notification.h"
#include "pluginsettingswidget.h"

#include <QHash>

namespace Snore
{

class SNORE_EXPORT SnorePlugin : public QObject
{
    Q_OBJECT
public:
    enum PluginType {
        NONE = 0,
        ALL = 0xFFFFF,//for loading plugins
        BACKEND = 0x1,
        SECONDARY_BACKEND = 0x2,
        FRONTEND = 0x4,
        PLUGIN = 0x8
    };
    Q_DECLARE_FLAGS(PluginTypes, PluginType)
    Q_ENUMS(PluginType)

    static PluginTypes typeFromString(const QString &t);
    static QString typeToString(const PluginTypes t);
    static const QList<PluginTypes> &types();

    SnorePlugin(const QString &name);
    virtual ~SnorePlugin();
    virtual bool initialize();
    virtual bool deinitialize();
    bool isInitialized() const;

    const QString &name() const;
    PluginTypes type() const;
    const QString typeName() const;

    QVariant value(const QString &key, SettingsType type = GLOBAL_SETTING) const;
    void setValue(const QString &key, const QVariant &value, SettingsType type = GLOBAL_SETTING);
    void setDefaultValue(const QString &key, const QVariant &value, SettingsType type = GLOBAL_SETTING);

    virtual PluginSettingsWidget *settingsWidget();

protected:
    virtual QString settingsVersion() const;

private:
    SnorePlugin() = delete;
    QString normaliseKey(const QString &key) const;

    QString m_name;
    bool m_initialized = false;
    PluginTypes m_type = NONE;

    friend class Notification;
    friend class PluginContainer;

};

}
Q_DECLARE_OPERATORS_FOR_FLAGS(Snore::SnorePlugin::PluginTypes)
Q_DECLARE_METATYPE(Snore::SnorePlugin::PluginTypes)

Q_DECLARE_INTERFACE(Snore::SnorePlugin,
                    "org.Snore.SnorePlugin/1.0")

SNORE_EXPORT QDebug operator<<(QDebug, const Snore::SnorePlugin::PluginTypes &);

SNORE_EXPORT QDataStream &operator<<(QDataStream &out, const Snore::SnorePlugin::PluginTypes &type);
SNORE_EXPORT QDataStream &operator>>(QDataStream &in, Snore::SnorePlugin::PluginTypes &type);

#endif//SNORE_PLUGINS_H
