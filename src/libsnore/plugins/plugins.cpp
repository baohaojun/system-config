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

#include "../snore.h"
#include "../snore_p.h"
#include "snorebackend.h"
#include "snorefrontend.h"
#include "../notification/notification_p.h"
#include "plugincontainer.h"

#include <QDebug>
#include <QMetaEnum>
#include <QGuiApplication>

using namespace Snore;

SnorePlugin::SnorePlugin()
{
    Q_ASSERT_X(thread() == qApp->thread(), Q_FUNC_INFO, "Plugin initialized in wrong thread");
    if (thread() != qApp->thread()) {
        qCWarning(SNORE) << "Plugin initialized in wrong thread.";
    }
}

SnorePlugin::~SnorePlugin()
{
    qCDebug(SNORE) << name() << this << "deleted";
}

bool SnorePlugin::isEnabled() const
{
    return m_enabled;
}

QVariant SnorePlugin::settingsValue(const QString &key, SettingsType type) const
{
    return SnoreCore::instance().settingsValue(normaliseKey(key), type);
}

void SnorePlugin::setSettingsValue(const QString &key, const QVariant &value, SettingsType type)
{
    SnoreCore::instance().setSettingsValue(normaliseKey(key), value, type);
}

void SnorePlugin::setDefaultSettingsValue(const QString &key, const QVariant &value, SettingsType type)
{
    SnoreCore::instance().setDefaultSettingsValue(normaliseKey(key), value, type);
}

QString SnorePlugin::normaliseKey(const QString &key) const
{
    return name() + QLatin1Char('-') + typeName() + QLatin1Char('/') + key + QLatin1Char('.') + settingsVersion();
}

const QString &SnorePlugin::name() const
{
    Q_ASSERT_X(m_container, Q_FUNC_INFO, "Plugin container not set.");
    return m_container->name();
}

SnorePlugin::PluginTypes SnorePlugin::type() const
{
    Q_ASSERT_X(m_container, Q_FUNC_INFO, "Plugin container not set.");
    return m_container->type();
}

const QString SnorePlugin::typeName() const
{
    return SnorePlugin::typeToString(type());
}

bool SnorePlugin::isReady()
{
    return m_error.isEmpty();
}

QString SnorePlugin::errorString() const
{
    return m_error;
}

QString SnorePlugin::settingsVersion() const
{
    return QStringLiteral("v1");
}

void SnorePlugin::setDefaultSettings()
{
    setDefaultSettingsValue(QStringLiteral("Enabled"), false, LOCAL_SETTING);
}

void SnorePlugin::setErrorString(const QString &_error)
{
    m_error = _error;
    qCWarning(SNORE) << name() << "encountered an error:" << m_error;
    disable();
    emit error(_error);
}

void SnorePlugin::setEnabled(bool enabled)
{
    if (enabled != m_enabled) {
        emit enabledChanged(enabled);
    }
    m_enabled = enabled;
}

void SnorePlugin::enable()
{
    setEnabled(true);
}

void SnorePlugin::disable()
{
    setEnabled(false);
}

SnorePlugin::PluginTypes SnorePlugin::typeFromString(const QString &t)
{
    return (SnorePlugin::PluginTypes)SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType")).keyToValue(t.toUpper().toLatin1().constData());
}

QString SnorePlugin::typeToString(const SnorePlugin::PluginTypes t)
{
    return QString::fromLatin1(SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType")).valueToKey(t));
}

const QList<SnorePlugin::PluginTypes> &SnorePlugin::types()
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

QDebug operator<<(QDebug debug, const Snore::SnorePlugin::PluginTypes &flags)
{
    QMetaEnum e = SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType"));
    debug.nospace() << "PluginTypes(";
    bool needSeparator = false;
    int key;
    for (int i = 0; i < e.keyCount(); ++i) {
        key = e.value(i);
        if (flags.testFlag((SnorePlugin::PluginType)key)) {
            if (needSeparator) {
                debug.nospace() << '|';
            } else {
                needSeparator = true;
            }

            debug.nospace() << e.valueToKey(key);
        }
    }
    debug << ')';
    return debug.space();

}

QDebug operator<<(QDebug debug, const Snore::SnorePlugin *p)
{
    debug.nospace() << p->metaObject()->className() << "(" << (void *)p  << ", " << p->name() << ")";
    return debug.space();
}

QDataStream &operator<<(QDataStream &out, const Snore::SnorePlugin::PluginTypes &type)
{
    out << static_cast<int>(type);
    return out;
}

QDataStream &operator>>(QDataStream &in, Snore::SnorePlugin::PluginTypes &type)
{
    int key;
    in >> key;
    type = static_cast<SnorePlugin::PluginTypes>(key);
    return in;
}
