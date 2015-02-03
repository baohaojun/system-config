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
#include "snore_exports.h"
#include "pluginsettingswidget.h"
#include "../notification/notification.h"

#include <QHash>

namespace Snore
{
class Application;
class SnoreCore;

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

    SnorePlugin(const QString &name);
    virtual ~SnorePlugin();
    virtual bool initialize();
    virtual bool deinitialize();
    bool isInitialized() const;
    SnoreCore *snore();
    const SnoreCore *snore() const;

    const QString &name() const;
    PluginTypes type() const;
    const QString typeName() const;


    QVariant value(const QString &key) const;
    void setValue(const QString &key, const QVariant &value);
    void setDefaultValue(const QString &key, const QVariant &value,const QString &help);

    virtual PluginSettingsWidget *settingsWidget();

private:
    SnorePlugin() {}
    QString normaliseKey(const QString &key) const;

    QString m_name;
    bool m_initialized = false;
    PluginTypes m_type = NONE;
    friend class PluginContainer;

};
Q_DECLARE_OPERATORS_FOR_FLAGS(Snore::SnorePlugin::PluginTypes)

}
Q_DECLARE_INTERFACE(Snore::SnorePlugin,
                    "org.Snore.SnorePlugin/1.0")

SNORE_EXPORT QDebug operator<< (QDebug, const Snore::SnorePlugin::PluginTypes &);

#endif//SNORE_PLUGINS_H
