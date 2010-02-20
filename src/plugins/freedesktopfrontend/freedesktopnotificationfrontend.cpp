#include "freedesktopnotificationfrontend.h"
#include "notificationsadaptor.h"
#include <QtCore>
#include <QtDBus>
#include <QImage>
#include "plugins/freedesktopnotification/fredesktopnotification.h"
#include "core/snoreserver.h"

Q_EXPORT_PLUGIN2(freedesktop_frontend,FreedesktopNotification_Frontend)

FreedesktopNotification_Frontend::FreedesktopNotification_Frontend(){
    setProperty("name","FreedesktopNotification_Frontend");
    new  NotificationsAdaptor(this);
    QDBusConnection dbus = QDBusConnection::sessionBus();
    dbus.registerService( "org.freedesktop.Notifications" );
    dbus.registerObject( "/org/freedesktop/Notifications", this );
}

FreedesktopNotification_Frontend::~FreedesktopNotification_Frontend(){
    QDBusConnection dbus = QDBusConnection::sessionBus();
    dbus.unregisterService( "org.freedesktop.Notifications" );
}

void FreedesktopNotification_Frontend::actionInvoked(QSharedPointer<Notification>notification){
    emit ActionInvoked(notification->id,QString::number(notification->actionInvoked));
}

void FreedesktopNotification_Frontend::notificationClosed(QSharedPointer<Notification>notification){
    uint reason;
    switch(notification->actionInvoked){
    case Notification::TIMED_OUT:
        reason=1;
        break;
    case Notification::ACTION_1:
    case Notification::ACTION_2:
    case Notification::ACTION_3:
        reason=2;
        break;
    default:
        reason=4;
    }

    emit NotificationClosed(notification->id,reason);
}

QString FreedesktopNotification_Frontend::getImagefromHint(const FreedesktopImageHint &img){
    QCryptographicHash hash(QCryptographicHash::Md5);
    hash.addData(img.imageData);
    QString filename=SnoreServer::snoreTMP+hash.result().toHex()+".png";
    QFile file(filename);
    if(file.exists())return filename;

    QImage qimage=img.toQImage();
    qimage.save(filename,"PNG");
    qDebug()<<"Saving to "<<filename;
    return filename;
}

uint FreedesktopNotification_Frontend::Notify(const QString &app_name, uint replaces_id,
                                              const QString &app_icon, const QString &summary, const QString &body,
                                              const QStringList &actions, const QVariantMap &hints, int timeout)
{
    qDebug()<<app_name<<summary<<body<<app_icon;
    QString icon;

    if(hints.contains("image_data")){
        FreedesktopImageHint image;
        hints["image_data"].value<QDBusArgument>()>>image;
        icon=getImagefromHint(image);
    }
    QSharedPointer<Notification> noti(new Notification(property("name").value<QString>(),summary,body,icon,timeout));
    noti->id=replaces_id;
    return getSnore()->broadcastNotification(noti);
}



void FreedesktopNotification_Frontend::CloseNotification(uint id){
    QSharedPointer<Notification> n(new Notification());
    n->id=id;
    getSnore()->closeNotification(n);
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
    vendor = "SnoreNotify";
    version = "0.01";
    specVersion = "0";
    return "SnoreNotify";
}


#include "freedesktopnotificationfrontend.moc"
