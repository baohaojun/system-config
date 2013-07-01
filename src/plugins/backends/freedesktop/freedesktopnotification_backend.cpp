#include "freedesktopnotification_backend.h"

#include <QtGlobal>
#include <QDebug>
#include "core/notification/notification.h"
#include <QtCore>
#include <QImage>
#include "fredesktopnotification.h"
#include "core/snore.h"


using namespace Snore;

Q_EXPORT_PLUGIN2 ( freedesktopnotificationbackend,FreedesktopBackend )

static const char dbusServiceName[] = "org.freedesktop.Notifications";
static const char dbusInterfaceName[] = "org.freedesktop.Notifications";
static const char dbusPath[] = "/org/freedesktop/Notifications";


FreedesktopBackend::FreedesktopBackend () :
    SnoreBackend ( "FreedesktopNotification_Backend")
{


}

bool FreedesktopBackend::init(SnoreCore *snore){
      QDBusConnection::sessionBus().connect ( "org.freedesktop.Notifications","/org/freedesktop/Notifications","org.freedesktop.Notifications","ActionInvoked",this,SLOT ( actionInvoked( uint,QString ) ) );
    //    if ( getVendor() =="GNOME" )
    QDBusConnection::sessionBus().connect ( "org.freedesktop.Notifications","/org/freedesktop/Notifications","org.freedesktop.Notifications","NotificationClosed",this,SLOT ( closed ( uint,uint ) ) );
    return SnoreBackend::init(snore);
}

void FreedesktopBackend::registerApplication ( Application *application )
{
    Q_UNUSED ( application );
}

void FreedesktopBackend::unregisterApplication ( Application *application )
{
    Q_UNUSED ( application );
}

uint  FreedesktopBackend::notify ( Notification noti )
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
    uint id = 0;
    if(replyMsg.type() == QDBusMessage::ReplyMessage){
        id= replyMsg.arguments().at(0).toUInt();
        qDebug()<<"DBUS_ID"<<id;
        m_activeNotifications[id] = noti;
    }
    return id;
}
void FreedesktopBackend::actionInvoked(const uint &id, const QString &actionID){
    Notification noti = m_activeNotifications[id];
    if(noti.id() == 0)
        return;
    qDebug() <<"Action"<<id<<"|"<<actionID ;
    noti.setActionInvoked ( actionID == "default"?1:actionID.toInt() );
    snore()->notificationActionInvoked ( noti );
}

void FreedesktopBackend::closeNotification ( Notification notification )
{
    m_activeNotifications.remove(notification.id());
    QDBusMessage message = QDBusMessage::createMethodCall(dbusServiceName, dbusPath, dbusInterfaceName,"CloseNotification");
    QVariantList args;
    args<<notification.id();
    message.setArguments(args);
    QDBusConnection::sessionBus().send(message);
}



void FreedesktopBackend::closed ( const uint &id,const uint &reason )
{
    qDebug() <<"Closed"<<id<<"|"<<reason;
    if(id == 0)
        return;
    Notification noti =  m_activeNotifications.take(id);
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






