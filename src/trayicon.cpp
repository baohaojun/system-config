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
#include "core/snore.h"
#include "core/snore_p.h"

#include <QSystemTrayIcon>
#include <QMenu>
#include <QAction>

#include "core/version.h"

using namespace Snore;

TrayIcon::TrayIcon():
    m_trayIcon(new QSystemTrayIcon(QIcon(":/root/snore.png"))),
    m_app("SnoreNotify Test", Icon(":/root/snore.png")),
    m_alert("Default", Icon(":/root/snore.png"))
{
    m_app.addAlert(m_alert);
}

void TrayIcon::initConextMenu(SnoreCore *snore)
{
    m_snore = snore;
    m_trayIcon->setVisible(true);

    m_trayMenu = new QMenu("SnoreNotify");
    QString version = QString("SnoreNotify %1").arg(Version::version());
    if (Version::revision() != "") {
        version += QString("-%1").arg(Version::revision());
    }
    m_trayMenu->addAction(version);
    m_trayMenu->addSeparator();
    m_trayMenu->addAction("Test Notification", this, SLOT(slotTestNotification()));
    m_trayMenu->addSeparator();
    m_backendActions =  new QActionGroup(m_trayMenu);
    m_backendActions->setExclusive(true);
    foreach(const QString & back, m_snore->notificationBackends()) {
        QAction *b = m_trayMenu->addAction(back, this, SLOT(setPrimaryBackend()));
        b->setCheckable(true);
        if (back == m_snore->primaryNotificationBackend()) {
            b->setChecked(true);
        }
        m_backendActions->addAction(b);
    }
    m_trayMenu->addSeparator();
    m_trayMenu->addAction("Exit", qApp, SLOT(quit()));

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

void TrayIcon::setPrimaryBackend()
{
    QAction *a = qobject_cast<QAction *>(sender());
    m_snore->setPrimaryNotificationBackend(a->text());

    foreach(QAction * action, m_backendActions->actions()) {
        if (action->text() == m_snore->primaryNotificationBackend()) {
            action->setChecked(true);
            break;
        }
    }
}

void TrayIcon::slotTestNotification()
{

    if (!m_snore->aplications().contains(m_app.name())) {
        m_snore->registerApplication(m_app);
    }
    Notification noti(m_app, m_alert, "Hello World",
                      "<i>This is Snore</i><br>"
                      "<a href=\"https://github.com/TheOneRing/Snorenotify\">Project Website</a><br>"
                      "1<br>"
                      "2<br>"
                      "3<br>"
                      "4<br>"
                      "5<br>", Icon(":/root/snore.png"));
    noti.addAction(Action(1, "Test Action"));
    m_snore->broadcastNotification(noti);

    QTimer *timer = new QTimer(this);
    m_notifications[timer] = noti;
    timer->setSingleShot(true);
    timer->setInterval(noti.timeout() / 2 * 1000);
    connect(timer, SIGNAL(timeout()), this, SLOT(sloutUpdateTestNotification()));
    timer->start();

    //    m_snore->deregisterApplication(app);
}

void TrayIcon::sloutUpdateTestNotification()
{
    QTimer *timer = qobject_cast<QTimer *>(sender());
    Notification noti = m_notifications.take(timer);
    Notification update(noti, "Hello World",
                        "<b>This is Snore</b><br>"
                        "<u>This icon is quite a long line of text, isnt it I think it is what do you think? btw the icon should be in color</u><br>"
                        "<a href=\"https://github.com/TheOneRing/Snorenotify\">Project Website</a>", Icon("http://jweatherwatch.googlecode.com/svn/trunk/iconset/04.png"));
    m_snore->broadcastNotification(update);
    timer->deleteLater();
}

