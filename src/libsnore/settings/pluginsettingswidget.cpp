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
#include "pluginsettingswidget.h"
#include "libsnore/plugins/plugins.h"
#include "snore.h"

#include <QCheckBox>
#include <QLabel>

using namespace Snore;

PluginSettingsWidget::PluginSettingsWidget(SnorePlugin *snorePlugin, QWidget *parent) :
    QWidget(parent),
    m_snorePlugin(snorePlugin),
    m_layout(new QFormLayout),
    m_enabled(new QCheckBox)
{
    setLayout(m_layout);
    if (m_snorePlugin->type() != SnorePlugin::Backend) {
        // backends are handled through a combo box.
        addRow(tr("Enabled:"), m_enabled);
    }

}

PluginSettingsWidget::~PluginSettingsWidget()
{

}

const QString PluginSettingsWidget::name() const
{
    return m_snorePlugin->name();
}

void PluginSettingsWidget::addRow(const QString &label, QWidget *widget, const QString &toolTip)
{
    QLabel *lb = new QLabel(label, this);
    m_layout->addRow(lb, widget);
    if (!toolTip.isEmpty()) {
        widget->setToolTip(toolTip);
        lb->setToolTip(toolTip);
    }
}

void PluginSettingsWidget::loadSettings()
{
    if (m_snorePlugin->type() != SnorePlugin::Backend) {
        m_enabled->setChecked(m_snorePlugin->settingsValue(QStringLiteral("Enabled"), LocalSettings).toBool());
    }
    load();
}

void PluginSettingsWidget::saveSettings()
{
    if (m_snorePlugin->type() != SnorePlugin::Backend) {
        m_snorePlugin->setSettingsValue(QStringLiteral("Enabled"), m_enabled->isChecked(), LocalSettings);
    }
    save();
}

bool PluginSettingsWidget::isDirty()
{
    return m_dirty;
}

QVariant PluginSettingsWidget::settingsValue(const QString &key, SettingsType type) const
{
    return m_snorePlugin->settingsValue(key, type);
}

void PluginSettingsWidget::setSettingsValue(const QString &key, const QVariant &value, SettingsType type)
{
    if (this->settingsValue(key) != value) {
        m_snorePlugin->setSettingsValue(key, value, type);
        m_dirty = true;
    }
}

void PluginSettingsWidget::load()
{

}

void PluginSettingsWidget::save()
{

}

