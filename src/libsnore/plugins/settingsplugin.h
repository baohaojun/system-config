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

#ifndef SETTINGSPLUGIN_H
#define SETTINGSPLUGIN_H


#include "libsnore/snore_exports.h"
#include "libsnore/plugins/plugins.h"

namespace Snore {

class PluginSettingsWidget;  

class SNORE_EXPORT SettingsPlugin :  public SnorePlugin
{
Q_OBJECT
Q_INTERFACES(Snore::SnorePlugin)
public:
SettingsPlugin();
~SettingsPlugin();

PluginTypes type() const override{
  return SnorePlugin::SETTINGS;
};

virtual PluginSettingsWidget *settingsWidget(SnorePlugin *parent) = 0;

};

}


Q_DECLARE_INTERFACE(Snore::SettingsPlugin,
                    "org.Snore.SettingsPlugin/1.0")

#define SNORE_DECLARE_SETTINGS_PLUGIN(NAME)\
class NAMESettings : public Snore::SettingsPlugin{\
    Q_OBJECT\
    Q_INTERFACES(Snore::SettingsPlugin)\
    Q_PLUGIN_METADATA(IID "org.Snore.SettingsPlugin/1.0" FILE "plugin.json")\
public:\
  Snore::PluginSettingsWidget *settingsWidget(Snore::SnorePlugin *parent) override{\
      return new NAME(parent);\
}\
};
  

#endif // SETTINGSPLUGIN_H
