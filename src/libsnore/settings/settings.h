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
#ifndef SETTINGS_H
#define SETTINGS_H

#include "libsnore/settings/snore_settings_exports.h"
#include "libsnore/plugins/plugins.h"
#include "libsnore/settings/pluginsettingswidget.h"

namespace Snore
{

class SNORE_SETTINGS_EXPORT Settings
{
public:
    /**
       *
       * @return A list of widgets a settings dialog.
       */
    static QList<PluginSettingsWidget*> settingWidgets ( SnorePlugin::PluginTypes type );

};
}

#endif // SETTINGS_H
