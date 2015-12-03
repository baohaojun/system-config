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
#ifndef PLUGINSETTINGSWIDGET_H
#define PLUGINSETTINGSWIDGET_H

#include "libsnore/settings/snore_settings_exports.h"
#include "libsnore/snoreglobals.h"

#include <QWidget>
#include <QFormLayout>
#include <QCheckBox>

namespace Snore
{
class SnorePlugin;

class SNORE_SETTINGS_EXPORT PluginSettingsWidget : public QWidget
{
    Q_OBJECT
public:
    explicit PluginSettingsWidget(SnorePlugin *snorePlugin, QWidget *parent = nullptr);
    ~PluginSettingsWidget();

    const QString name() const;

    void addRow(const QString &label, QWidget *widget, const QString &toolTip = QString());

    void loadSettings();
    void saveSettings();

    bool isDirty();

protected:
    QVariant settingsValue(const QString &key, Snore::SettingsType type = Snore::GlobalSetting) const;
    void setSettingsValue(const QString &key, const QVariant &settingsValue, Snore::SettingsType type = Snore::GlobalSetting);

    virtual void load();
    virtual void save();

private:
    SnorePlugin *m_snorePlugin;
    QFormLayout *m_layout;
    QCheckBox *m_enabled;
    bool m_dirty = false;

};
}

#endif // PLUGINSETTINGSWIDGET_H
