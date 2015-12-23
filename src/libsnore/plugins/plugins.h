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

#ifndef SNORE_PLUGINS_H
#define SNORE_PLUGINS_H
#include "libsnore/snore_exports.h"
#include "libsnore/snoreglobals.h"
#include "libsnore/notification/notification.h"
#include "libsnore/hint.h"

#include <QHash>

namespace Snore
{

class PluginContainer;

/**
 *  SnorePlugin represents the base class for the plugins.
 *
 * @author Hannah von Reth \<vonreth at kde.org\>
 */

class SNORE_EXPORT SnorePlugin : public QObject
{
    Q_OBJECT
public:
    /**
     * The PluginType Flags.
     */
    enum PluginType {
        /**
         * Flag for loading no plugins.
         */
        None                = 0,

        /**
         * Backends are those plugins that are capable of reporting user interaction.
         */
        Backend             = 1 << 0,

        /**
         * Secondary backaends are non interactive.
         * Redirection or playback of a sound file.
         */
        SecondaryBackend   = 1 << 1,

        /**
         * Frontends are capable of recieving notifications.
         */
        Frontend            = 1 << 2,

        /**
         * General plugins, currently there are no plugins implemented.
         */
        Plugin              = 1 << 3,

        /**
         * A settings page for a Plugin.
         */
        Settings    = 1 << 4,

        /**
         * Flag for loading all plugins.
         */
        All                 = ~0
    };

    Q_DECLARE_FLAGS(PluginTypes, PluginType)
    Q_ENUMS(PluginType)

    static PluginTypes typeFromString(const QString &t);
    static QString typeToString(const PluginTypes t);
    static const QList<Snore::SnorePlugin::PluginTypes> &types();

    SnorePlugin();
    virtual ~SnorePlugin();

    /**
     * Sets the enabled state of the plugin to @param enabled .
     */
    void setEnabled(bool enabled);

    /**
     * Enables the plugin.
     */
    void enable();

    /**
     * Disables the plugin.
     */
    void disable();

    /**
     * Returns whether the Plugin is enabled.
     */
    bool isEnabled() const;

    /**
     * Returns the name of the plugin.
     */
    const QString &name() const;

    /**
     * Returns the plugin type.
     */
    virtual PluginTypes type() const = 0;

    /**
     * Returns the name of the plugin type.
     */
    const QString typeName() const;

    virtual bool isReady();

    /**
     * Returns the error string or an empty string.
     */
    QString errorString() const;

    QVariant settingsValue(const QString &key, SettingsType type = GlobalSetting) const;
    void setSettingsValue(const QString &key, const QVariant &settingsValue, SettingsType type = GlobalSetting);
    void setDefaultSettingsValue(const QString &key, const QVariant &settingsValue, SettingsType type = GlobalSetting);

    const Hint &constHints() const;

Q_SIGNALS:
    void enabledChanged(bool enabled);
    void error(const QString &error);

protected:
    /**
     * Returns the version suffix used for the plugin settings.
     */
    virtual QString settingsVersion() const;

    /**
     * Set default setting values for the Plugin.
     */
    virtual void setDefaultSettings();

    void setErrorString(const QString &error);

    Hint &hints();
private:
    QString normaliseKey(const QString &key) const;
    void setDefaultSettingsPlugin();

    bool m_enabled = false;
    PluginContainer *m_container = nullptr;
    QString m_error;
    Hint m_hints;

    friend class PluginContainer;

};

}
Q_DECLARE_OPERATORS_FOR_FLAGS(Snore::SnorePlugin::PluginTypes)
Q_DECLARE_METATYPE(Snore::SnorePlugin::PluginTypes)

Q_DECLARE_INTERFACE(Snore::SnorePlugin,
                    "org.Snore.SnorePlugin/1.0")

SNORE_EXPORT QDebug operator<< (QDebug, const Snore::SnorePlugin::PluginTypes &);
SNORE_EXPORT QDebug operator<< (QDebug, const Snore::SnorePlugin *);

SNORE_EXPORT QDataStream &operator<< (QDataStream &out, const Snore::SnorePlugin::PluginTypes &type);
SNORE_EXPORT QDataStream &operator>> (QDataStream &in, Snore::SnorePlugin::PluginTypes &type);

#endif//SNORE_PLUGINS_H
