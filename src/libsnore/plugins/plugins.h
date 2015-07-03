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

class PluginContainer;

class SNORE_EXPORT SnorePlugin : public QObject
{
    Q_OBJECT
public:
    enum PluginType {
        NONE                = 0,
        BACKEND             = 1 << 0,
        SECONDARY_BACKEND   = 1 << 1,
        FRONTEND            = 1 << 2,
        PLUGIN              = 1 << 3,
        ALL                 = ~0
    };

    Q_DECLARE_FLAGS(PluginTypes, PluginType)
    Q_ENUMS(PluginType)

    static PluginTypes typeFromString(const QString &t);
    static QString typeToString(const PluginTypes t);
    static QList<PluginTypes> types();

    SnorePlugin();
    virtual ~SnorePlugin();

    // TODO: remove need of recursive calling of parent methode....
    virtual bool initialize();
    virtual bool deinitialize();
    bool isInitialized() const;

    const QString &name() const;
    PluginTypes type() const;
    const QString typeName() const;

    QVariant settingsValue(const QString &key, SettingsType type = GLOBAL_SETTING) const;
    void setSettingsValue(const QString &key, const QVariant &settingsValue, SettingsType type = GLOBAL_SETTING);
    void setDefaultSettingsValue(const QString &key, const QVariant &settingsValue, SettingsType type = GLOBAL_SETTING);

    virtual PluginSettingsWidget *settingsWidget();

protected:
    virtual QString settingsVersion() const;

private:
    QString normaliseKey(const QString &key) const;

    bool m_initialized = false;
    PluginContainer *m_container = nullptr;

    friend class PluginContainer;

};

}
Q_DECLARE_OPERATORS_FOR_FLAGS(Snore::SnorePlugin::PluginTypes)
Q_DECLARE_METATYPE(Snore::SnorePlugin::PluginTypes)

Q_DECLARE_INTERFACE(Snore::SnorePlugin,
                    "org.Snore.SnorePlugin/1.0")

SNORE_EXPORT QDebug operator<<(QDebug, const Snore::SnorePlugin::PluginTypes &);
SNORE_EXPORT QDebug operator<<(QDebug, const Snore::SnorePlugin *);

SNORE_EXPORT QDataStream &operator<<(QDataStream &out, const Snore::SnorePlugin::PluginTypes &type);
SNORE_EXPORT QDataStream &operator>>(QDataStream &in, Snore::SnorePlugin::PluginTypes &type);

#endif//SNORE_PLUGINS_H
