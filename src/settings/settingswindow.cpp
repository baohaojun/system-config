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
#include "settingswindow.h"
#include "ui_settingswindow.h"

#include "snore.h"
#include "snore_p.h"
#include "settingsdialog.h"
#include "utils.h"

#include <QApplication>
#include <QDialogButtonBox>

#include <iostream>

using namespace Snore;

SettingsWindow::SettingsWindow(const QString &appName, QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::SettingsWindow)
{
    ui->setupUi(this);
    ui->widget->show();

    if (appName == QLatin1String("global")) {
        QStringList list = knownApps();
        list.removeAll(qApp->applicationName());
        ui->comboBox->addItems(list);
    } else {
        if (!knownApps().contains(appName)) {
            std::wcerr << "Error: " << appName.toUtf8().constData() << " is not known to Snorenotify" << std::endl;
            exit(1);
        }
        ui->comboBox->deleteLater();
        ui->label->deleteLater();
        SnoreCorePrivate::instance()->setLocalSttingsPrefix(appName);
        setWindowTitle(tr("%1 Settings").arg(appName));
        ui->widget->initTabs();
        ui->widget->setVisible(true);
    }
}

SettingsWindow::~SettingsWindow()
{
    delete ui;
}

const QStringList SettingsWindow::knownApps()
{
    return allSettingsKeysWithPrefix(Utils::settingsVersionSchema() + QLatin1String("/LocalSettings"), settings(),
    [](QSettings & settings) {
        return settings.childGroups();
    });
}

QSettings &SettingsWindow::settings()
{
    return SnoreCorePrivate::instance()->settings();
}

void SettingsWindow::on_comboBox_currentIndexChanged(const QString &arg1)
{
    SnoreCorePrivate::instance()->setLocalSttingsPrefix(arg1);
    ui->widget->initTabs();
    ui->widget->setVisible(true);
}

void SettingsWindow::on_buttonBox_clicked(QAbstractButton *button)
{
    switch (ui->buttonBox->buttonRole(button)) {
    case QDialogButtonBox::AcceptRole:
        ui->widget->accept();
        qApp->processEvents();
        qApp->quit();
        break;
    case QDialogButtonBox::ApplyRole:
        ui->widget->accept();
        break;
    case QDialogButtonBox::ResetRole:
        ui->widget->reset();
        break;
    case QDialogButtonBox::RejectRole:
        qApp->quit();
        break;
    default:
        qCWarning(SNORE) << "unhandled role" << button->text() << ui->buttonBox->buttonRole(button);
    }
}
