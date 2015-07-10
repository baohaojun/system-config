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

#include "../snore.h"
#include "../snore_p.h"
#include "snorebackend.h"
#include "snorefrontend.h"
#include "../notification/notification_p.h"
#include "plugincontainer.h"

#include <QDebug>
#include <QMetaEnum>
#include <QApplication>

using namespace Snore;

SnorePlugin::SnorePlugin()
{
    Q_ASSERT_X(thread() == qApp->thread(), Q_FUNC_INFO, "Plugin initialized in wrong thread");
    if (thread() != qApp->thread()) {
        snoreDebug(SNORE_WARNING) << "Plugin initialized in wrong thread.";
    }
}

SnorePlugin::~SnorePlugin()
{
    snoreDebug(SNORE_DEBUG) << name() << this << "deleted";
}

bool SnorePlugin::initialize()
{
    setDefaultSettingsValue(QLatin1String("Enabled"), false, LOCAL_SETTING);
    if (m_initialized) {
        qFatal("Something went wrong, plugin %s is already initialized", this->name().toLatin1().constData());
        return false;
    }
    snoreDebug(SNORE_DEBUG) << "Initialize" << name() << this;
    m_initialized = true;
    return true;
}

bool SnorePlugin::isInitialized() const
{
    return m_initialized;
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

Snore::PluginSettingsWidget *SnorePlugin::settingsWidget()
{
    if (type() != SnorePlugin::BACKEND) {
        // don't display a useless default widget for backends
        return new PluginSettingsWidget(this);
    }
    return nullptr;
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

QString SnorePlugin::settingsVersion() const
{
    return QLatin1String("v1");
}

bool SnorePlugin::deinitialize()
{
    if (m_initialized) {
        snoreDebug(SNORE_DEBUG) << "Deinitialize" << name() << this;
        m_initialized = false;
        return true;
    }
    return false;
}

SnorePlugin::PluginTypes SnorePlugin::typeFromString(const QString &t)
{
    return (SnorePlugin::PluginTypes)SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType")).keyToValue(t.toUpper().toLatin1().constData());
}

QString SnorePlugin::typeToString(const SnorePlugin::PluginTypes t)
{
    return QString::fromLatin1(SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType")).valueToKey(t));
}

QList<SnorePlugin::PluginTypes> SnorePlugin::types()
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
