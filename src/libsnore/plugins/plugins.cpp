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

SnorePlugin::SnorePlugin(const QString &name) :
    m_name(name)
{
    Q_ASSERT_X(thread() != qApp->thread(), Q_FUNC_INFO, "Plugin initialized in wrong thread");
    if (thread() != qApp->thread()) {
        snoreDebug(SNORE_WARNING) << "Plugin initialized in wrong thread.";
    }
    setDefaultValue("Enabled", false, LOCAL_SETTING);
}

SnorePlugin::~SnorePlugin()
{
    snoreDebug(SNORE_DEBUG) << m_name << this << "deleted";
}

bool SnorePlugin::initialize()
{
    if (m_initialized) {
        qFatal("Something went wrong, plugin %s is already initialized", this->name().toLatin1().constData());
        return false;
    }
    snoreDebug(SNORE_DEBUG) << "Initialize" << m_name << this;
    m_initialized = true;
    return true;
}

bool SnorePlugin::isInitialized() const
{
    return m_initialized;
}

QVariant SnorePlugin::value(const QString &key, SettingsType type) const
{
    return SnoreCore::instance().value(normaliseKey(key), type);
}

void SnorePlugin::setValue(const QString &key, const QVariant &value, SettingsType type)
{
    SnoreCore::instance().setValue(normaliseKey(key), value, type);
}

void SnorePlugin::setDefaultValue(const QString &key, const QVariant &value, SettingsType type)
{
    SnoreCore::instance().setDefaultValue(normaliseKey(key), value, type);
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
    return QString("%1/%2.%3").arg(m_name, key, settingsVersion());
}

const QString &SnorePlugin::name() const
{
    return m_name;
}

SnorePlugin::PluginTypes SnorePlugin::type() const
{
    return m_type;
}

const QString SnorePlugin::typeName() const
{
    return SnorePlugin::typeToString(m_type);
}

QString SnorePlugin::settingsVersion() const
{
    return "v1";
}

bool SnorePlugin::deinitialize()
{
    if (m_initialized) {
        snoreDebug(SNORE_DEBUG) << "Deinitialize" << m_name << this;
        m_initialized = false;
        return true;
    }
    return false;
}

SnorePlugin::PluginTypes SnorePlugin::typeFromString(const QString &t)
{
    return (SnorePlugin::PluginTypes)SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType")).keyToValue(t.toUpper().toLatin1());
}

QString SnorePlugin::typeToString(const SnorePlugin::PluginTypes t)
{
    return SnorePlugin::staticMetaObject.enumerator(SnorePlugin::staticMetaObject.indexOfEnumerator("PluginType")).valueToKey(t);
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

QDebug operator<<(QDebug debug, const Snore::SnorePlugin *p){
    debug.nospace() << p->metaObject()->className() << "(" << (void*)p  << ", " << p->name() << ")";
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
