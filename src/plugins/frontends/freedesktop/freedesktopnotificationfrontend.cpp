/****************************************************************************************
 * Copyright (c) 2010-2012 Patrick von Reth <patrick.vonreth@gmail.com>                 *
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

#include "plugins/backends/freedesktop/fredesktopnotification.h"
#include "core/snore.h"

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
    emit ActionInvoked(notification.id(),QString::number(notification.actionInvoked()->id));
}

void FreedesktopFrontend::notificationClosed(Notification notification) {

    qDebug()<<"Closing Dbus notification"<<notification.id()<<"reason:"<<(int)notification.closeReason();
    emit NotificationClosed(notification.id(),notification.closeReason());
}

uint FreedesktopFrontend::Notify(const QString &app_name, uint replaces_id,
                                 const QString &app_icon, const QString &summary, const QString &body,
                                 const QStringList &actions, const QVariantMap &hints, int timeout)
{
    qDebug()<<app_name<<summary<<body<<app_icon<<timeout;
    SnoreIcon icon;
    NotificationEnums::Prioritys::prioritys priotity = NotificationEnums::Prioritys::NORMAL;

    if(hints.contains("image_data")){
        FreedesktopImageHint image;
        hints["image_data"].value<QDBusArgument>()>>image;
        icon = SnoreIcon(image.toQImage());
    }

    if(!snore()->aplications().contains(app_name)){
#ifdef HAVE_KDE
        SnoreIcon appIcon = SnoreIcon(KIconLoader::global()->iconPath(app_icon, KIconLoader::Desktop));
#else
        SnoreIcon appIcon = SnoreIcon(":/root/images/freedesktop-dbus.png");
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
               //            << "body-hyperlinks"
               //            << "body-markup"
            << "icon-static"
            << "actions"
               ;
}

QString FreedesktopFrontend::GetServerInformation(QString& vendor, QString& version, QString& specVersion)
{
    vendor = "Snore";
    version = snore()->version();
    specVersion = "0";
    return "Snore";
}

#include "freedesktopnotificationfrontend.moc"
