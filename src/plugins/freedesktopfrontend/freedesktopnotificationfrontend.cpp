/****************************************************************************************
* Copyright (c) 2010 Patrick von Reth <patrick.vonreth@gmail.com>                      *
*                                                                                      *
* This program is free software; you can redistribute it and/or modify it under        *
* the terms of the GNU General Public License as published by the Free Software        *
* Foundation; either version 2 of the License, or (at your option) any later           *
* version.                                                                             *
*                                                                                      *
* This program is distributed in the hope that it will be useful, but WITHOUT ANY      *
* WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A      *
* PARTICULAR PURPOSE. See the GNU General Public License for more details.             *
*                                                                                      *
* You should have received a copy of the GNU General Public License along with         *
* this program.  If not, see <http://www.gnu.org/licenses/>.                           *
****************************************************************************************/

#include "freedesktopnotificationfrontend.h"
#include "notificationsadaptor.h"

#include "plugins/freedesktopnotification/fredesktopnotification.h"
#include "core/snoreserver.h"

#include <QtCore>
#include <QtDBus>
#include <QImage>

#ifdef HAVE_KDE
#include <KIcon>
#endif
using namespace Snore;

Q_EXPORT_PLUGIN2(freedesktop_frontend,FreedesktopNotification_Frontend)

FreedesktopNotification_Frontend::FreedesktopNotification_Frontend(SnoreServer *snore):
    Notification_Frontend("FreedesktopNotification_Frontend",snore)
{
    new  NotificationsAdaptor(this);
    QDBusConnection dbus = QDBusConnection::sessionBus();
    dbus.registerService( "org.freedesktop.Notifications" );
    dbus.registerObject( "/org/freedesktop/Notifications", this );
}

FreedesktopNotification_Frontend::~FreedesktopNotification_Frontend(){
    QDBusConnection dbus = QDBusConnection::sessionBus();
    dbus.unregisterService( "org.freedesktop.Notifications" );
}

void FreedesktopNotification_Frontend::actionInvoked(Notification notification) {
    emit ActionInvoked(notification.id(),QString::number(notification.actionInvoked()->id));
}

void FreedesktopNotification_Frontend::notificationClosed(Notification notification) {

    qDebug()<<"Closing Dbus notification"<<notification.id()<<"reason:"<<(int)notification.closeReason();
    activeNotifications.remove(notification.id());
    qDebug()<<"Active Dbus Notifications"<<activeNotifications.keys();
    emit NotificationClosed(notification.id(),notification.closeReason());
}

uint FreedesktopNotification_Frontend::Notify(const QString &app_name, uint replaces_id,
                                              const QString &app_icon, const QString &summary, const QString &body,
                                              const QStringList &actions, const QVariantMap &hints, int timeout)
{
    qDebug()<<app_name<<summary<<body<<app_icon;
    SnoreIcon icon;
    NotificationEnums::Prioritys::prioritys priotity = NotificationEnums::Prioritys::NORMAL;

    if(hints.contains("image_data")){
        FreedesktopImageHint image;
        hints["image_data"].value<QDBusArgument>()>>image;
        icon = SnoreIcon(image.toQImage());
    }
    if(!snore()->aplications().contains(app_name)){
        SnoreIcon appIcon;
#ifdef HAVE_KDE
        KIcon kicon(app_icon);
        appIcon = SnoreIcon(kicon.pixmap(100,100).toImage());
#else
        appIcon = SnoreIcon(":/root/images/freedesktop-dbus.png");
#endif
        Application *a = new Application(app_name,appIcon);
        a->addAlert(new Alert("DBus Alert","DBus Alert",appIcon));
        snore()->addApplication(a);
        snore()->applicationIsInitialized(a);
    }

    if (hints.contains("urgency")) {
        priotity =  NotificationEnums::Prioritys::prioritys(hints["urgency"].toInt()-1);
    }

    Notification noti(app_name,"DBus Alert",summary,body,icon,timeout==-1?Notification::DefaultTimeout:timeout/1000,replaces_id,priotity);
    noti.setSource(this);
    for(int i = 0;i < actions.length(); i+=2){
        noti.addAction(new Notification::Action(actions.at(i).toInt(),actions.at(i+1)));
    }

    snore()->broadcastNotification(noti);
    activeNotifications[noti.id()] = noti;
    startTimeout(noti.id(),noti.timeout());
    return noti.id();
}



void FreedesktopNotification_Frontend::CloseNotification(uint id){
    Notification noti = activeNotifications.take(id);
    qDebug()<<"Active Dbus Notifications"<<activeNotifications.keys();
    snore()->closeNotification(noti,NotificationEnums::CloseReasons::TIMED_OUT);
}

QStringList FreedesktopNotification_Frontend::GetCapabilities()
{
    return QStringList()
            << "body"
               //            << "body-hyperlinks"
               //            << "body-markup"
            << "icon-static"
            << "actions"
               ;
}

QString FreedesktopNotification_Frontend::GetServerInformation(QString& vendor, QString& version, QString& specVersion)
{
    vendor = "Snore";
    version = snore()->version();
    specVersion = "0";
    return "Snore";
}

#include "freedesktopnotificationfrontend.moc"
