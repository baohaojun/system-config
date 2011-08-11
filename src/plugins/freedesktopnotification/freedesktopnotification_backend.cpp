#include "freedesktopnotification_backend.h"

#include <QtGlobal>
#include <QDebug>
#include "core/notification/notification.h"
#include <QtCore>
#include <QImage>
#include "fredesktopnotification.h"
#include "core/snoreserver.h"
#include <QtDBus>

Q_EXPORT_PLUGIN2 ( freedesktopnotificationbackend,FreedesktopNotification_Backend )

static const char dbusServiceName[] = "org.freedesktop.Notifications";
static const char dbusInterfaceName[] = "org.freedesktop.Notifications";
static const char dbusPath[] = "/org/freedesktop/Notifications";


FreedesktopNotification_Backend::FreedesktopNotification_Backend ( SnoreServer *snore ) :
    Notification_Backend ( "FreedesktopNotification_Backend",snore )
{
    QDBusConnection::sessionBus().connect ( "org.freedesktop.Notifications","/org/freedesktop/Notifications","org.freedesktop.Notifications","ActionInvoked",this,SLOT ( actionInvoked( uint,QString ) ) );
    //    if ( getVendor() =="GNOME" )
    QDBusConnection::sessionBus().connect ( "org.freedesktop.Notifications","/org/freedesktop/Notifications","org.freedesktop.Notifications","NotificationClosed",this,SLOT ( closed ( uint,uint ) ) );

}

bool FreedesktopNotification_Backend::isPrimaryNotificationBackend()
{
    return true;
}

void FreedesktopNotification_Backend::registerApplication ( Application *application )
{
    Q_UNUSED ( application );
}

void FreedesktopNotification_Backend::unregisterApplication ( Application *application )
{
    Q_UNUSED ( application );
}

uint  FreedesktopNotification_Backend::notify ( Notification noti )
{    
    QDBusMessage message = QDBusMessage::createMethodCall(dbusServiceName, dbusPath, dbusInterfaceName,  "Notify");
    QVariantList args;

    args<<noti.application() ; // app_name
    args<<noti.id() ; // replaces_id
    args<<"" ; // app_icon
    args<<noti.title() ; // summary
    args<<noti.text() ; // body

    QStringList actions;
    foreach(int k,noti.actions().keys()){
        actions<<QString::number(k)<<noti.actions()[k]->name;
    }
    args<<actions;

    if(!noti.icon().isEmpty()){
        QVariantMap image_data;
        QVariant img = QVariant::fromValue(FreedesktopImageHint(noti.icon().image().scaledToWidth(50,Qt::FastTransformation)));
        image_data.insert(QString("image_data"),img);
        args<<image_data;
    }

    args<<noti.timeout()*1000;

    message.setArguments(args);

    QDBusMessage replyMsg = QDBusConnection::sessionBus().call(message);
    uint id ;
    if(replyMsg.type() == QDBusMessage::ReplyMessage){
        id= replyMsg.arguments().at(0).toUInt();
        qDebug()<<"DBUS_ID"<<id;
        activeNotifications[id] = noti;
        startTimeout(id,noti.timeout());
    }
    return id;
}
void FreedesktopNotification_Backend::actionInvoked(const uint &id, const QString &actionID){
    Notification noti = activeNotifications[id];
    qDebug() <<"Action"<<id<<"|"<<actionID ;
    noti.setActionInvoked ( actionID == "default"?1:actionID.toInt() );
    snore()->notificationActionInvoked ( noti );
}

void FreedesktopNotification_Backend::closeNotification ( Notification notification )
{
    activeNotifications.remove(notification.id());
    QDBusMessage message = QDBusMessage::createMethodCall(dbusServiceName, dbusPath, dbusInterfaceName,"CloseNotification");
    QVariantList args;
    args<<notification.id();
    message.setArguments(args);
    QDBusConnection::sessionBus().send(message);
}



void FreedesktopNotification_Backend::closed ( const uint &id,const uint &reason )
{
    qDebug() <<"Closed"<<id<<"|"<<reason;
    if(id == 0)
        return;
    Notification noti =  activeNotifications.take(id);
    snore()->closeNotification ( noti ,QFlag(reason));
}

//QString fNotification::getVendor()
//{
//    if ( vendor == "" )
//    {
//        QDBusMessage recive =  notificationInterface.call ( QDBus::AutoDetect,"GetServerInformation" );
//        vendor=recive.arguments() [1].toString();
//        qDebug() <<recive.arguments();
//    }
//    return vendor;
//}






