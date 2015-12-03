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

#include "settingsdialog.h"
#include "ui_settingsdialog.h"
#include "snore.h"
#include "snore_p.h"
#include "settings.h"

#include <QTabWidget>
#include <QCheckBox>

using namespace Snore;

SettingsDialog::SettingsDialog(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::SettingsDialog)
{
    ui->setupUi(this);
    initTabs();
}

SettingsDialog::~SettingsDialog()
{
    delete ui;
}

void SettingsDialog::initTabs()
{
    SnorePlugin::PluginTypes types = SnoreCore::instance().settingsValue(QStringLiteral("PluginTypes"), LocalSetting).value<SnorePlugin::PluginTypes>();
    if (types == SnorePlugin::None) {
        types = SnorePlugin::All;
    }
    auto addWidgets = [&](QTabWidget * target, QWidget * container, SnorePlugin::PluginTypes type) {
        bool enabled = false;
        target->clear();
        if (types & type) {
            foreach(PluginSettingsWidget * widget, Settings::settingWidgets(type)) {
                target->addTab(widget, widget->name());
                m_tabs.append(widget);
                enabled = true;
            }
        }
        if (enabled) {
            if (ui->tabWidget->indexOf(container) == -1) {
                ui->tabWidget->addTab(container, container->property("TAB_NAME").toString());
            }
        } else {
            int index = ui->tabWidget->indexOf(container);
            container->setProperty("TAB_NAME", ui->tabWidget->tabText(index));
            ui->tabWidget->removeTab(index);
        }
    };
    addWidgets(ui->tabWidget_backends, ui->tab_backends, SnorePlugin::Backend);
    addWidgets(ui->tabWidget_secondary_backends, ui->tab_secondaryBackends, SnorePlugin::SecondaryBackend);
    addWidgets(ui->tabWidget_frontends, ui->tab_frontends,  SnorePlugin::Frontend);
    addWidgets(ui->tabWidget_plugins, ui->tab_plugins, SnorePlugin::Plugin);

    ui->errorLabel->setVisible(false);
    ui->errorLineEdit->setVisible(false);

    connect(&SnoreCore::instance(), &SnoreCore::primaryNotificationBackendChanged, this, &SettingsDialog::loadPrimaryBackendBox);
    connect(&SnoreCore::instance(), &SnoreCore::primaryNotificationBackendError, [this](const QString & error) {
        ui->errorLabel->setVisible(true);
        ui->errorLineEdit->setVisible(true);
        ui->errorLineEdit->setText(error);
    });
}

void SettingsDialog::on_pushButton_clicked()
{
    SnoreCore::instance().displayExampleNotification();
}

void SettingsDialog::load()
{
    qCDebug(SNORE) << "loading";
    loadPrimaryBackendBox(SnoreCore::instance().settingsValue(QStringLiteral("PrimaryBackend"), LocalSetting).toString());
    ui->timeoutSpinBox->setValue(SnoreCore::instance().settingsValue(QStringLiteral("Timeout"), LocalSetting).toInt());
    ui->disableNotificationSoundCheckBox->setChecked(SnoreCore::instance().settingsValue(QStringLiteral("Silent"), LocalSetting).toBool());
    foreach(auto widget, m_tabs) {
        widget->loadSettings();
    }
}

void SettingsDialog::loadPrimaryBackendBox(const QString &backend)
{
    if (SnoreCore::instance().settingsValue(QStringLiteral("PluginTypes"), LocalSetting).value<SnorePlugin::PluginTypes>() & SnorePlugin::Backend) {
        ui->primaryBackendComboBox->clear();
        QStringList list = SnoreCore::instance().pluginNames(SnorePlugin::Backend);
        ui->primaryBackendComboBox->addItems(list);
        ui->primaryBackendComboBox->setCurrentIndex(list.indexOf(backend));
        ui->primaryBackendComboBox->setVisible(true);
        ui->primaryBackendLabel->setVisible(true);
    } else {
        ui->primaryBackendComboBox->setVisible(false);
        ui->primaryBackendLabel->setVisible(false);
    }
}

void SettingsDialog::save()
{
    qCDebug(SNORE) << "saving";
    bool dirty = false;
    foreach(auto w, m_tabs) {
        w->saveSettings();
        dirty |= w->isDirty();
    }
    dirty |= SnoreCore::instance().settingsValue(QStringLiteral("PrimaryBackend"), LocalSetting).toString() != ui->primaryBackendComboBox->currentText();
    dirty |= SnoreCore::instance().settingsValue(QStringLiteral("Timeout"), LocalSetting).toInt() != ui->timeoutSpinBox->value();
    dirty |= SnoreCore::instance().settingsValue(QStringLiteral("Silent"), LocalSetting).toBool() != ui->disableNotificationSoundCheckBox->isChecked();

    SnoreCore::instance().setSettingsValue(QStringLiteral("PrimaryBackend"), ui->primaryBackendComboBox->currentText(), LocalSetting);
    SnoreCore::instance().setSettingsValue(QStringLiteral("Timeout"), ui->timeoutSpinBox->value(), LocalSetting);
    SnoreCore::instance().setSettingsValue(QStringLiteral("Silent"), ui->disableNotificationSoundCheckBox->isChecked(), LocalSetting);

    if (dirty) {
        SnoreCorePrivate::instance()->syncSettings();
    }
}

void SettingsDialog::setVisible(bool b)
{
    if (b) {
        load();
    }
    QWidget::setVisible(b);
}

void SettingsDialog::accept()
{
    save();
}

void SettingsDialog::reset()
{
    load();
}

