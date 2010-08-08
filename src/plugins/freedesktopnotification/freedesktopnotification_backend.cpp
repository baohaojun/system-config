#include "freedesktopnotification_backend.h"

#include <QtGlobal>
#include <QDebug>
#include "core/notification.h"
#include <QtCore>
#include <QImage>
#include "fredesktopnotification.h"
#include "core/snoreserver.h"
#include <QtDBus>

Q_EXPORT_PLUGIN2 ( freedesktopnotificationbackend,FreedesktopNotification_Backend )

FreedesktopNotification_Backend::FreedesktopNotification_Backend ( SnoreServer *snore ) :
        Notification_Backend ( "FreedesktopNotification_Backend",snore )
{
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

int  FreedesktopNotification_Backend::notify ( QSharedPointer<Notification> noti )
{
    fNotification *n = new fNotification ( noti , this);
    qDebug()<<"Sending Notification wit Freedesktop_Backend"<<noti->title()<<noti->text();
    uint out = n->send();
    n->deleteLater();
    return out;
}

void FreedesktopNotification_Backend::closeNotification ( QSharedPointer<Notification> notification )
{
    //TODO: fix
    fNotification *fn = new fNotification ( notification,this );
    fn->close();
}


QString fNotification::vendor ( "" );

QDBusInterface fNotification::notificationInterface ( "org.freedesktop.Notifications","/org/freedesktop/Notifications","org.freedesktop.Notifications" );

fNotification::fNotification(QSharedPointer< Notification > notification, FreedesktopNotification_Backend* parent):
        QObject(parent),
        _notification(notification)
{}



uint fNotification::send()
{
    Q_ASSERT(!_notification.isNull());
    FreedesktopNotification n ( _notification );
    QDBusMessage recive=notificationInterface.call ( "Notify", QVariant::fromValue ( n ) );
    uint id=recive.arguments().last().toInt();
    selfdistruct = new QTimer(this );
    selfdistruct->setSingleShot ( true );
    connect ( selfdistruct, SIGNAL ( timeout() ), this, SLOT ( close() ) );
    selfdistruct->start ( _notification->timeout() *1000 );
    QDBusConnection::sessionBus().connect ( "org.freedesktop.Notifications","/org/freedesktop/Notifications","org.freedesktop.Notifications","ActionInvoked",this,SLOT ( action ( uint,QString ) ) );
    if ( getVendor() =="GNOME" )
        QDBusConnection::sessionBus().connect ( "org.freedesktop.Notifications","/org/freedesktop/Notifications","org.freedesktop.Notifications","NotificationClosed",this,SLOT ( closed ( uint,uint ) ) );
    return id;
}

void fNotification::action ( const uint &id, const QString &action_key )
{
    if ( id!=_notification->id() ) return;
    close();
    qDebug() <<id<<"|"<<action_key ;

    notificationInterface.call ( QDBus::AutoDetect,"CloseNotification",id );
    //default is only working on gnome


    _notification->setActionInvoked ( action_key=="default"?Notification::ACTION_1:Notification::actions ( action_key.toInt() ) );
    parent->snore()->notificationActionInvoked ( _notification );
    selfDelete();
}

void fNotification::closed ( const uint &id,const uint &reason )
{
    qDebug() <<id<<"|"<<reason;;
    if ( id!=_notification->id() ) return;
    close();
    if ( reason==1 )
        _notification->setActionInvoked ( Notification::TIMED_OUT );
    if ( reason==2 )
        _notification->setActionInvoked ( Notification::CLOSED );
    parent->snore()->closeNotification ( _notification );
    selfDelete();
}



void fNotification::close()
{
    blockSignals ( true );
    if ( !selfdistruct->isActive() )
    {
        _notification->setActionInvoked ( Notification::TIMED_OUT );
        parent->snore()->closeNotification ( _notification );
        selfDelete();
    }
    selfdistruct->stop();
}

QString fNotification::getVendor()
{
    if ( vendor == "" )
    {
        QDBusMessage recive =  notificationInterface.call ( QDBus::AutoDetect,"GetServerInformation" );
        vendor=recive.arguments() [1].toString();
        qDebug() <<recive.arguments();
    }
    return vendor;
}
void fNotification::selfDelete()
{
    _notification.clear();
    deleteLater();
}



