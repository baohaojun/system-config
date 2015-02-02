/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2015  Patrick von Reth <vonreth@kde.org>

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
#ifndef PLUGINSETTINGSWIDGET_H
#define PLUGINSETTINGSWIDGET_H

#include "snore_exports.h"

#include <QWidget>


namespace Snore
{
class SnorePlugin;

class SNORE_EXPORT PluginSettingsWidget : public QWidget
{
    Q_OBJECT
public:
    explicit PluginSettingsWidget(SnorePlugin *snorePlugin, QWidget *parent = nullptr);
    ~PluginSettingsWidget();

    virtual void load() = 0;
    virtual void save() = 0;

    const QString name() const;

protected:
    SnorePlugin *m_snorePlugin;
};
}

#endif // PLUGINSETTINGSWIDGET_H
