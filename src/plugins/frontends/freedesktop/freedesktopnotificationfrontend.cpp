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

bool FreedesktopFrontend::init(SnoreCore *snore){
    new  NotificationsAdaptor(this);
    QDBusConnection dbus = QDBusConnection::sessionBus();
    dbus.registerService( "org.freedesktop.Notifications" );
    dbus.registerObject( "/org/freedesktop/Notifications", this );
    return SnoreFrontend::init(snore);
}

void FreedesktopFrontend::actionInvoked(Notification notification) {
    if(notification.actionInvoked())
    {
        emit ActionInvoked(notification.id(),QString::number(notification.actionInvoked()->id));
    }
}

void FreedesktopFrontend::notificationClosed(Notification notification) {

    qDebug()<<"Closing Dbus notification"<<notification<<"reason:"<<(int)notification.closeReason();
    emit NotificationClosed(notification.id(),notification.closeReason());
}

uint FreedesktopFrontend::Notify(const QString &app_name, uint replaces_id,
                                 const QString &app_icon, const QString &summary, const QString &body,
                                 const QStringList &actions, const QVariantMap &hints, int timeout)
{
    Icon icon;
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

    if(!snore()->aplications().contains(app_name)){
#ifdef HAVE_KDE
        Icon appIcon(KIconLoader::global()->iconPath(app_icon, KIconLoader::Desktop));
#else
        Icon appIcon(":/root/images/freedesktop-dbus.png");
#endif
        Application *a = new Application(app_name,appIcon);
        a->addAlert(new Alert("DBus Alert","DBus Alert",appIcon));
        snore()->registerApplication(a);
    }

    if (hints.contains("urgency")) {
        priotity =  NotificationEnums::Prioritys::prioritys(hints["urgency"].toInt()-1);
    }

    Notification noti(app_name,"DBus Alert",summary,body,icon,timeout==-1?Notification::DefaultTimeout:timeout/1000,priotity);
    if(replaces_id != 0)
    {
        noti.setUpdateID(replaces_id);
    }
    noti.setSource(this);
    for(int i = 0;i < actions.length(); i+=2){
        noti.addAction(new Notification::Action(actions.at(i).toInt(),actions.at(i+1)));
    }

    snore()->broadcastNotification(noti);
    startTimeout(noti.id(),noti.timeout());
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
