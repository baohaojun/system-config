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
    if (thread() != qApp->thread()) {
        moveToThread(qApp->thread());
    }
    setDefaultValue("Enabled", true, "Whether the plugin is enabled");
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

QVariant SnorePlugin::value(const QString &key) const
{
    return SnoreCore::instance().settings()->value(normaliseKey(key));
}

void SnorePlugin::setValue(const QString &key, const QVariant &value)
{
    SnoreCore::instance().settings()->setValue(normaliseKey(key), value);
}

void SnorePlugin::setDefaultValue(const QString &key, const QVariant &value, const QString &help)
{
    QString pk(normaliseKey(key));
    SnoreCorePrivate::instance()->addSettingsDescription(pk, help);
    if (!SnoreCore::instance().settings()->contains(pk)) {
        SnoreCore::instance().settings()->setValue(normaliseKey(key), value);
    }
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
    return QString("%1/%2").arg(m_name, key);
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
    return PluginContainer::typeToString(m_type);
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

QDebug operator <<(QDebug debug, const Snore::SnorePlugin::PluginTypes &flags)
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
