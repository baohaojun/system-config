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
#include "core/snore.h"
#include "core/version.h"
#include "core/notification/notification_p.h"

#include <QtCore>
#include <QtDBus>
#include <QImage>

#ifdef HAVE_KDE
#include <KIconLoader>
#endif
using namespace Snore;

Q_EXPORT_PLUGIN2(freedesktop,FreedesktopFrontend)

FreedesktopFrontend::FreedesktopFrontend():
    SnoreFrontend("Freedesktop")
{


}

FreedesktopFrontend::~FreedesktopFrontend(){
    QDBusConnection dbus = QDBusConnection::sessionBus();
    dbus.unregisterService( "org.freedesktop.Notifications" );
}

bool FreedesktopFrontend::initialize(SnoreCore *snore)
{
    m_adaptor = new  NotificationsAdaptor(this);
    QDBusConnection dbus = QDBusConnection::sessionBus();
    if(dbus.registerService( "org.freedesktop.Notifications" ) &&
            dbus.registerObject( "/org/freedesktop/Notifications", this ))
    {
        return SnoreFrontend::initialize(snore);
    }
    return false;
}

bool FreedesktopFrontend::deinitialize()
{
    if(SnoreFrontend::deinitialize())
    {
        QDBusConnection dbus = QDBusConnection::sessionBus();
        dbus.unregisterService("org.freedesktop.Notifications" );
        dbus.unregisterObject("/org/freedesktop/Notifications" );
        m_adaptor->deleteLater();
        m_adaptor = NULL;
        return true;
    }
    return false;
}

void FreedesktopFrontend::actionInvoked(Notification notification)
{
    if(notification.actionInvoked().isValid())
    {
        emit ActionInvoked(notification.id(),QString::number(notification.actionInvoked().id()));
    }
}

void FreedesktopFrontend::notificationClosed(Notification notification)
{
    emit NotificationClosed(notification.id(),notification.closeReason());
}

uint FreedesktopFrontend::Notify(const QString &app_name, uint replaces_id,
                                 const QString &app_icon, const QString &summary, const QString &body,
                                 const QStringList &actions, const QVariantMap &hints, int timeout)
{
    Icon icon;
    Application app;
    NotificationEnums::Prioritys::prioritys priotity = NotificationEnums::Prioritys::NORMAL;

    if(hints.contains("image_data")){
        FreedesktopImageHint image;
        hints["image_data"].value<QDBusArgument>()>>image;
        icon = Icon(image.toQImage());
    }
    else
    {
        icon = Icon(":/root/images/freedesktop-dbus.png");
    }

    if(!snore()->aplications().contains(app_name))
    {
#ifdef HAVE_KDE
        Icon appIcon(KIconLoader::global()->iconPath(app_icon, KIconLoader::Desktop));
#else
        Icon appIcon(":/root/images/freedesktop-dbus.png");
#endif
        Alert alert("DBus Alert","DBus Alert",appIcon);
        app = Application(app_name,appIcon);
        app.addAlert(alert);
        snore()->registerApplication(app);
    }
    else
    {
        app = snore()->aplications()[app_name];
    }

    if (hints.contains("urgency")) {
        priotity =  NotificationEnums::Prioritys::prioritys(hints["urgency"].toInt()-1);
    }

    Notification noti(app, *app.alerts().begin(), summary, body, icon, timeout==-1?Notification::defaultTimeout():timeout/1000, priotity);
    if(replaces_id != 0)
    {
        noti.setNotificationToReplace(snore()->getActiveNotificationByID(replaces_id));
    }
    noti.data()->setSource(this);
    for(int i = 0;i < actions.length(); i+=2)
    {
        noti.addAction(Action(actions.at(i).toInt(),actions.at(i+1)));
    }

    snore()->broadcastNotification(noti);
    startTimeout(noti);
    return noti.id();
}



void FreedesktopFrontend::CloseNotification(uint id){
    Notification noti = snore()->getActiveNotificationByID(id);
    if(noti.isValid())
    {
        snore()->requestCloseNotification(noti,NotificationEnums::CloseReasons::TIMED_OUT);
    }
}

QStringList FreedesktopFrontend::GetCapabilities()
{
    return QStringList()
            << "body"
            << "urgency"
               //            << "body-hyperlinks"
            << "body-markup"
            << "icon-static"
            << "actions";
}

QString FreedesktopFrontend::GetServerInformation(QString& vendor, QString& version, QString& specVersion)
{
    vendor = "Snore";
    version = Version::version();
    specVersion = "0";
    return "Snore";
}
