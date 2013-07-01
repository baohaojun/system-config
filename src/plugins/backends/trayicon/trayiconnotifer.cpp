#include "trayiconnotifer.h"
#include "core/snore.h"

#include <QtCore>
#include <QSystemTrayIcon>
#include <QTimer>
#include <QTime>
#include <QDebug>
using namespace Snore;

Q_EXPORT_PLUGIN2(trayicon,TrayIconNotifer)

TrayIconNotifer::TrayIconNotifer () :
    SnoreBackend ( "SystemTray" ),
    m_id ( 0 ),
    m_displayed(-1),
    m_trayIcon(NULL)
{

}

bool TrayIconNotifer::init(SnoreCore *snore){    
    m_trayIcon = snore->trayIcon();
    if(m_trayIcon == NULL)
        return false;
    connect(m_trayIcon,SIGNAL(messageClicked()),this,SLOT(actionInvoked()));
    return SnoreBackend::init(snore);
}

void TrayIconNotifer::registerApplication ( Application *application )
{
    Q_UNUSED ( application )
}
void TrayIconNotifer::unregisterApplication ( Application *application )
{
    Q_UNUSED ( application )
}

uint TrayIconNotifer::notify ( Notification notification )
{
    m_notificationQue.append(notification);
    if(m_lastNotify.elapsed()> Notification::DefaultTimeout * 1000){
        displayNotification();
    }
    return m_id++;
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
    qDebug()<<"Display"<<m_notificationQue.size();
    Notification notification =  m_notificationQue.takeFirst();
    if(!m_notificationQue.isEmpty()){
        QTimer::singleShot(notification.timeout()*1000,this,SLOT(closeNotification()));
    }

    qDebug()<<"taking"<<notification.title();
    m_displayed = notification.id();
    m_activeNotifications.insert(notification.id(),notification);
    m_trayIcon->showMessage ( Notification::toPlainText(notification.title()),Notification::toPlainText(notification.text()),QSystemTrayIcon::NoIcon,notification.timeout() *1000 );
    m_lastNotify.restart();
}

void TrayIconNotifer::closeNotification(){
    if(m_activeNotifications.contains(m_displayed)){
        Notification noti = m_activeNotifications.take(m_displayed);
        snore()->closeNotification(noti,NotificationEnums::CloseReasons::TIMED_OUT);
    }
    displayNotification();
}

void TrayIconNotifer::actionInvoked(){
    qDebug()<<"Traicon invoked"<<m_displayed;
    if(m_activeNotifications.contains(m_displayed)){
        Notification noti = m_activeNotifications.take(m_displayed);
        if(noti.actions().isEmpty()){
            noti.setActionInvoked(noti.actions().keys().first());
            snore()->notificationActionInvoked(noti);
        }
        snore()->closeNotification(noti,NotificationEnums::CloseReasons::CLOSED);

    }

}


#include "trayiconnotifer.moc"
