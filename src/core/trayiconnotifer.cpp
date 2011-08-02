#include "trayiconnotifer.h"
#include "snoreserver.h"

#include <QSystemTrayIcon>
#include <QTimer>
#include <QTime>
#include <QDebug>

TrayIconNotifer::TrayIconNotifer ( SnoreServer *snore, QSystemTrayIcon *icon ) :
    Notification_Backend ( "SystemTray",snore ),
    _trayIcon ( icon ),
    _id ( 0 ),
    _displayed(-1)
{
    connect(_trayIcon,SIGNAL(messageClicked()),this,SLOT(actionInvoked()));
}

void TrayIconNotifer::registerApplication ( Application *application )
{
    Q_UNUSED ( application )
}
void TrayIconNotifer::unregisterApplication ( Application *application )
{
    Q_UNUSED ( application )
}

int TrayIconNotifer::notify ( Notification notification )
{
    _notificationQue.append(notification);
    if(_lastNotify.elapsed()> Notification::DefaultTimeout * 1000){
        displayNotification();
    }
    return _id++;
}

void TrayIconNotifer::closeNotification ( Notification notification )
{
    Q_UNUSED ( notification )
}

bool TrayIconNotifer::isPrimaryNotificationBackend()
{
    return true;
}

void TrayIconNotifer::displayNotification(){
    qDebug()<<"Display"<<_notificationQue.size();
    Notification notification =  _notificationQue.takeFirst();
    if(!_notificationQue.isEmpty()){
        QTimer::singleShot(notification.timeout()*1000,this,SLOT(closeNotification()));
    }

    qDebug()<<"taking"<<notification.title();
    _displayed = notification.id();
    activeNotifications.insert(notification.id(),notification);
    _trayIcon->showMessage ( Notification::toPlainText(notification.title()),Notification::toPlainText(notification.text()),QSystemTrayIcon::NoIcon,notification.timeout() *1000 );
    _lastNotify.restart();
}

void TrayIconNotifer::closeNotification(){
    if(activeNotifications.contains(_displayed)){
        Notification noti = activeNotifications.take(_displayed);
        snore()->closeNotification(noti,NotificationEnums::CloseReasons::TIMED_OUT);
    }
    displayNotification();
}

void TrayIconNotifer::actionInvoked(){
    qDebug()<<"Traicon invoked"<<_displayed;
    if(activeNotifications.contains(_displayed)){
        Notification noti = activeNotifications.take(_displayed);
        if(noti.actions().isEmpty()){
            noti.setActionInvoked(noti.actions().keys().first());
            snore()->notificationActionInvoked(noti);
        }
        snore()->closeNotification(noti,NotificationEnums::CloseReasons::CLOSED);

    }

}

#include "trayiconnotifer.moc"
