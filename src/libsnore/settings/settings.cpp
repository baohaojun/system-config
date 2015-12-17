/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2015  Hannah von Reth <vonreth@kde.org>

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
#include "settings.h"
#include "libsnore/snore_p.h"
#include "libsnore/plugins/settingsplugin.h"
#include "libsnore/settings/pluginsettingswidget.h"

#include <QList>

using namespace Snore;

QList<PluginSettingsWidget*> Settings::settingWidgets(SnorePlugin::PluginTypes type)
{
    SnoreCorePrivate* core(SnoreCorePrivate::instance());
    QList<PluginSettingsWidget*> list;
    foreach (const QString & name, core->m_pluginNames[type]) {
        //TODO: mem leak?

        SnorePlugin* plugin = core->m_plugins[qMakePair(type, name)];
        SettingsPlugin* settingsPlugin = qobject_cast< Snore::SettingsPlugin* >(core->m_plugins[qMakePair(Snore::SnorePlugin::Settings, name + SnorePlugin::typeToString(type))]);
        if (settingsPlugin) {
            PluginSettingsWidget* widget = settingsPlugin->settingsWidget(plugin);
            if (widget) {
                list.append(widget);
            }
        } else {
            if (!qobject_cast< Snore::SnoreBackend* >(plugin)) {
                list.append(new PluginSettingsWidget(plugin));
            }
        }
    }
    qSort(list.begin(), list.end(), [](PluginSettingsWidget * a, PluginSettingsWidget * b) {
        return a->name() < b->name();
    });
    return list;
}
