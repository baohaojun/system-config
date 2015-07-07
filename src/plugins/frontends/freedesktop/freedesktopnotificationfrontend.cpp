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

#include "freedesktopnotificationfrontend.h"
#include "notificationsadaptor.h"

#include "plugins/backends/freedesktop/fredesktopnotification.h"
#include "libsnore/snore.h"
#include "libsnore/version.h"
#include "libsnore/notification/notification_p.h"

#include <QImage>
#include <QIcon>

using namespace Snore;

bool FreedesktopFrontend::initialize()
{

    if (!SnoreFrontend::initialize()) {
        return false;
    }
    m_adaptor = new  NotificationsAdaptor(this);
    QDBusConnection dbus = QDBusConnection::sessionBus();
    if (dbus.registerService(QLatin1String("org.freedesktop.Notifications"))) {
        if (dbus.registerObject(QLatin1String("/org/freedesktop/Notifications"), this)) {
            return true;
        } else {
            snoreDebug(SNORE_WARNING) << "Failed to initialize" << name() << "failed to register object";
        }
    } else {
        snoreDebug(SNORE_WARNING) << "Failed to initialize" << name() << "failed to register service";
    }
    return false;
}

bool FreedesktopFrontend::deinitialize()
{
    if (SnoreFrontend::deinitialize()) {
        QDBusConnection dbus = QDBusConnection::sessionBus();
        dbus.unregisterService(QLatin1String("org.freedesktop.Notifications"));
        dbus.unregisterObject(QLatin1String("/org/freedesktop/Notifications"));
        m_adaptor->deleteLater();
        m_adaptor = nullptr;
        return true;
    }
    return false;
}

void FreedesktopFrontend::slotActionInvoked(Notification notification)
{

    if (notification.isActiveIn(this)) {
        if (notification.actionInvoked().isValid()) {
            emit ActionInvoked(notification.id(), QString::number(notification.actionInvoked().id()));
        }
    }
}

void FreedesktopFrontend::slotNotificationClosed(Notification notification)
{
    if (notification.removeActiveIn(this)) {
        emit NotificationClosed(notification.id(), notification.closeReason());
    }
}

uint FreedesktopFrontend::Notify(const QString &app_name, uint replaces_id,
                                 const QString &app_icon, const QString &summary, const QString &body,
                                 const QStringList &actions, const QVariantMap &hints, int timeout)
{
    Icon icon = Icon::defaultIcon();
    Application app;
    Notification::Prioritys priotity = Notification::NORMAL;

    if (hints.contains(QLatin1String("image_data"))) {
        FreedesktopImageHint image;
        hints.value(QLatin1String("image_data")).value<QDBusArgument>() >> image;
        icon = Icon(image.toQImage());
    }

    if (!SnoreCore::instance().aplications().contains(app_name)) {
        qDebug() << QIcon::themeSearchPaths();
        QIcon qicon = QIcon::fromTheme(app_icon, QIcon(QLatin1String(":/root/snore.png")));
        QSize max;
        foreach (const QSize &s, qicon.availableSizes()) {
            if (s.width()*s.height() > max.width()*max.height()) {
                max = s;
            }
        }
        Icon appIcon(qicon.pixmap(max).toImage());
        app = Application(app_name, appIcon);
        app.hints().setValue("use-markup", true);
        SnoreCore::instance().registerApplication(app);
    } else {
        app = SnoreCore::instance().aplications()[app_name];
    }

    if (hints.contains(QLatin1String("urgency"))) {
        priotity =  Notification::Prioritys(hints.value(QLatin1String("urgency")).toInt() - 1);
    }

    Notification noti;
    Notification toReplace = SnoreCore::instance().getActiveNotificationByID(replaces_id);
    if (replaces_id != 0 && toReplace.isValid()) {
        noti = Notification(toReplace, summary, body, icon, timeout == -1 ? Notification::defaultTimeout() : timeout / 1000, priotity);
    } else {
        noti = Notification(app, app.defaultAlert(), summary, body, icon, timeout == -1 ? Notification::defaultTimeout() : timeout / 1000, priotity);
    }
    for (int i = 0; i < actions.length(); i += 2) {
        noti.addAction(Action(actions.at(i).toInt(), actions.at(i + 1)));
    }

    noti.addActiveIn(this);
    SnoreCore::instance().broadcastNotification(noti);
    return noti.id();
}

void FreedesktopFrontend::CloseNotification(uint id)
{
    Notification noti = SnoreCore::instance().getActiveNotificationByID(id);
    if (noti.isValid()) {
        SnoreCore::instance().requestCloseNotification(noti, Notification::TIMED_OUT);
    }
}

QStringList FreedesktopFrontend::GetCapabilities()
{
    return QStringList()
           << QLatin1String("body")
           << QLatin1String("urgency")
           << QLatin1String("body-hyperlinks")
           << QLatin1String("body-markup")
           << QLatin1String("icon-static")
           << QLatin1String("actions");
}

QString FreedesktopFrontend::GetServerInformation(QString &vendor, QString &version, QString &specVersion)
{
    vendor = QLatin1String("SnoreNotify");
    version = Version::version();
    specVersion = QLatin1String("0.9");
    return vendor;
}
