#include "trayiconnotifer.h"

#include <QSystemTrayIcon>
#include <QTimer>
#include <QDebug>

TrayIconNotifer::TrayIconNotifer ( SnoreServer *snore, QSystemTrayIcon *icon ) :
        Notification_Backend ( "TrayiconNotifer",snore ),
        _trayIcon ( icon ),
        _noNotificationDisplayed(true),
        _id ( 0 )
{}

void TrayIconNotifer::registerApplication ( Application *application )
{
    Q_UNUSED ( application )
    }
void TrayIconNotifer::unregisterApplication ( Application *application )
{
    Q_UNUSED ( application )
    }

int TrayIconNotifer::notify ( QSharedPointer<Notification> notification )
{
    _notificationQue.append(notification);
    qDebug()<<"appending"<<notification->title();
    if(_noNotificationDisplayed){
        _noNotificationDisplayed = false;
        displayNotification();
    }
    return _id++;
}

void TrayIconNotifer::closeNotification ( QSharedPointer<Notification> notification )
{
    Q_UNUSED ( notification )
    }

bool TrayIconNotifer::isPrimaryNotificationBackend()
{
    return true;
}
//TODO:fix display of all notifications
void TrayIconNotifer::displayNotification(){
    qDebug()<<"Display"<<_notificationQue.size();
    if(_notificationQue.isEmpty()){
        _noNotificationDisplayed = true;
        return;
    }
    QSharedPointer<Notification> notification =  _notificationQue.takeLast();
    qDebug()<<"taking"<<notification->title();
    _trayIcon->showMessage ( Notification::toPlainText(notification->title()),Notification::toPlainText(notification->text()),QSystemTrayIcon::NoIcon,notification->timeout() *1000 );
    QTimer *t = new QTimer(notification.data());
    t->setInterval(notification->timeout() *1000);
    connect(t,SIGNAL(timeout()),this,SLOT(displayNotification()));
    t->start();
}

#include "trayiconnotifer.moc"
