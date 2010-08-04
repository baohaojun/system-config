#include "freedesktopnotification_backend.h"
#include <QDebug>
#include "core/notification.h"
#include <QtCore>
#include <QImage>
#include "fredesktopnotification.h"
#include "core/snoreserver.h"
#include <QtDBus>

Q_EXPORT_PLUGIN2(freedesktopnotificationbackend,FreedesktopNotification_Backend)

FreedesktopNotification_Backend::FreedesktopNotification_Backend(SnoreServer *snore):
Notification_Backend("FreedesktopNotification_Backend",snore)
{
}

bool FreedesktopNotification_Backend::isPrimaryNotificationBackend(){
    return true;
}

void FreedesktopNotification_Backend::registerApplication(Application *application){
    Q_UNUSED(application);
}

void FreedesktopNotification_Backend::unregisterApplication(Application *application){
    Q_UNUSED(application);
}

int  FreedesktopNotification_Backend::notify( QSharedPointer<Notification> noti){
    fNotification *n = new fNotification(this);
    qDebug()<<noti;
    n->notification = noti;
    return n->send();
}

void FreedesktopNotification_Backend::closeNotification(QSharedPointer<Notification> notification){
    //TODO: fix
        fNotification *fn=new fNotification(this);
        fn->notification=notification;
        fn->close();
}


QString fNotification::vendor("");

QDBusInterface fNotification::notificationInterface("org.freedesktop.Notifications","/org/freedesktop/Notifications","org.freedesktop.Notifications");

fNotification::fNotification(FreedesktopNotification_Backend* parent):parent(parent){
    setParent(parent);
}



uint fNotification::send(){
    qDebug("Sending a notification");
    FreedesktopNotification n(notification.data());
    QDBusMessage recive=notificationInterface.call("Notify", QVariant::fromValue(n));
    uint id=recive.arguments().last().toInt();

    selfdistruct.setParent(this);
    selfdistruct.setSingleShot(true);
    connect(&selfdistruct, SIGNAL(timeout()), this, SLOT(close()));
    selfdistruct.start(notification->timeout()*1000);
    QDBusConnection::sessionBus().connect("org.freedesktop.Notifications","/org/freedesktop/Notifications","org.freedesktop.Notifications","ActionInvoked",this,SLOT(action(uint,QString)));
    if(getVendor()=="GNOME")
        QDBusConnection::sessionBus().connect("org.freedesktop.Notifications","/org/freedesktop/Notifications","org.freedesktop.Notifications","NotificationClosed",this,SLOT(closed(uint,uint)));
    return id;
}

void fNotification::action(const uint &id, const QString &action_key){
    if(id!=notification->id())return;
    close();
    qDebug()<<id<<"|"<<action_key ;

    notificationInterface.call(QDBus::AutoDetect,"CloseNotification",id);
    //default is only working on gnome


    notification->setActionInvoked(action_key=="default"?Notification::ACTION_1:Notification::actions(action_key.toInt()));
    parent->snore()->notificationActionInvoked(notification);
    selfDelete();
}

void fNotification::closed(const uint &id,const uint &reason){
    qDebug()<<id<<"|"<<reason;;
    if(id!=notification->id())return;
    close();
    if(reason==1)
        notification->setActionInvoked(Notification::TIMED_OUT);
    if(reason==2)
        notification->setActionInvoked(Notification::CLOSED);
    parent->snore()->closeNotification(notification);
    selfDelete();
}



void fNotification::close(){
    blockSignals(true);
    if( !selfdistruct.isActive()){
        notification->setActionInvoked(Notification::TIMED_OUT);
        parent->snore()->closeNotification(notification);
        selfDelete();
    }
    selfdistruct.stop();
}

QString fNotification::getVendor(){
    if(vendor==""){
        QDBusMessage recive=  notificationInterface.call(QDBus::AutoDetect,"GetServerInformation");
        vendor=recive.arguments()[1].toString();
        qDebug()<<recive.arguments();
    }
    return vendor;
}
void fNotification::selfDelete(){
    notification.clear();
    deleteLater();
}



