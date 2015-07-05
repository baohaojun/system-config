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

#include "trayicon.h"
#include "libsnore/settingsdialog.h"
#include "libsnore/snore.h"
#include "libsnore/snore_p.h"

#include <QAction>
#include <QApplication>
#include <QDialogButtonBox>
#include <QMenu>
#include <QSystemTrayIcon>

#include "libsnore/version.h"

using namespace Snore;

TrayIcon::TrayIcon():
    m_trayIcon(new QSystemTrayIcon(QIcon(QLatin1String(":/root/snore.png"))))
{
    SnoreCorePrivate::instance()->defaultApplication().hints().setValue("use-markup", true);
    SnoreCorePrivate::instance()->defaultApplication().hints().setValue("tray-icon", QVariant::fromValue(QPointer<QSystemTrayIcon>(m_trayIcon)));
}

void TrayIcon::initConextMenu()
{
    m_settings = new SettingsDialog();
    QDialogButtonBox *box = new QDialogButtonBox(QDialogButtonBox::Apply | QDialogButtonBox::Cancel | QDialogButtonBox::Ok | QDialogButtonBox::Reset, m_settings);
    connect(box, &QDialogButtonBox::clicked, [ &, box](QAbstractButton * button) {
        switch (box->buttonRole(button)) {
        case QDialogButtonBox::AcceptRole:
            m_settings->accept();
            m_settings->setVisible(false);
            break;
        case QDialogButtonBox::ApplyRole:
            m_settings->accept();
            break;
        case QDialogButtonBox::ResetRole:
            m_settings->reset();
            break;
        case QDialogButtonBox::RejectRole:
            m_settings->setVisible(false);
            break;
        default:
            snoreDebug(SNORE_WARNING) << "unhandled role" << button->text() << box->buttonRole(button);
        }
    });
    m_settings->layout()->addWidget(box);

//    connect(m_settings, &SettingsDialog::finished, m_settings, &SettingsDialog::hide);

    m_trayIcon->setVisible(true);

    m_trayMenu = new QMenu(QLatin1String("SnoreNotify"));
    QString version = QLatin1String("SnoreNotify ") + Version::version();
    if (!Version::revision().isEmpty()) {
        version += QLatin1String("-") + Version::revision();
    }
    m_trayMenu->addAction(version);
    m_trayMenu->addSeparator();
    m_trayMenu->addAction(tr("Display Test Notification"), this, SLOT(slotTestNotification()));
    m_trayMenu->addSeparator();
    m_trayMenu->addAction(tr("Settings"), this, SLOT(slotSettings()));
    m_trayMenu->addSeparator();
    m_trayMenu->addAction(tr("Exit"), qApp, SLOT(quit()));

    m_trayIcon->setContextMenu(m_trayMenu);
}

void TrayIcon::hide()
{
    m_trayIcon->setVisible(false);
}

QSystemTrayIcon *TrayIcon::trayIcon()
{
    return m_trayIcon;
}

void TrayIcon::slotTestNotification()
{
    SnoreCore::instance().displayExapleNotification();
}

void TrayIcon::slotSettings()
{
    m_settings->show();
}

