/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2015  Patrick von Reth <vonreth@kde.org>

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

#include <QAbstractButton>
#include <QTabWidget>
#include <QFormLayout>
#include <QCheckBox>

using namespace Snore;

SettingsDialog::SettingsDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::SettingsDialog),
    m_app(new Application("SnoreSettings", Icon(":/root/snore.png"))),
    m_alert(new Alert("Test", Icon(":/root/snore.png")))
{
    ui->setupUi(this);

    m_app->addAlert(*m_alert);

    for (auto widget : SnoreCore::instance().settingWidgets()) {
        ui->tabWidget->addTab(widget, widget->name());
        m_tabs.append(widget);
    }
    load();
}

SettingsDialog::~SettingsDialog()
{
    delete ui;
}

void Snore::SettingsDialog::on_pushButton_clicked()
{
    if (!SnoreCore::instance().aplications().contains(m_app->name())) {
        SnoreCore::instance().registerApplication(*m_app);
    }
    Notification noti(*m_app, *m_alert, "Hello World",
                      "<i>This is Snore</i><br>"
                      "<a href=\"https://github.com/TheOneRing/Snorenotify\">Project Website</a><br>",
                      Icon(":/root/snore.png"));
    noti.addAction(Action(1, "Test Action"));
    SnoreCore::instance().broadcastNotification(noti);
}

void SettingsDialog::load()
{
    snoreDebug(SNORE_DEBUG) << "loading";
    ui->primaryBackendComboBox->clear();
    QStringList list = SnoreCore::instance().pluginNames(SnorePlugin::BACKEND);
    ui->primaryBackendComboBox->addItems(list);
    ui->primaryBackendComboBox->setCurrentIndex(list.indexOf(SnoreCore::instance().primaryNotificationBackend()));
    for (auto widget : m_tabs) {
        widget->loadSettings();
    }
}

void SettingsDialog::save()
{
    snoreDebug(SNORE_DEBUG) << "saving";
    SnoreCorePrivate::instance()->setBackendIfAvailible(ui->primaryBackendComboBox->currentText());
    for (auto w : m_tabs) {
        w->saveSettings();
    }
}

void Snore::SettingsDialog::on_buttonBox_clicked(QAbstractButton *button)
{
    switch (ui->buttonBox->buttonRole(button)) {
    case QDialogButtonBox::AcceptRole:
    case QDialogButtonBox::ApplyRole:
        save();
        break;
    case QDialogButtonBox::ResetRole:
        load();
        break;
    case QDialogButtonBox::RejectRole:
        break;
    default:
        snoreDebug(SNORE_WARNING) << "unhandled role" << button->text();
    }
}
