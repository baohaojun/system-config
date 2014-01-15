#include "trayiconnotifer.h"
#include "core/snore.h"
#include "core/snore_p.h"

#include <QtCore>
#include <QSystemTrayIcon>
#include <QTimer>
#include <QTime>
#include <QDebug>
using namespace Snore;

Q_EXPORT_PLUGIN2(trayicon,TrayIconNotifer)

TrayIconNotifer::TrayIconNotifer () :
    SnoreBackend ( "SystemTray",false,false ),
    m_trayIcon(NULL),
    m_displayed(-1)
{

}

bool TrayIconNotifer::initialize(SnoreCore *snore){
    m_trayIcon = snore->trayIcon();
    if(m_trayIcon == NULL)
    {
        return false;
    }
    connect(m_trayIcon,SIGNAL(messageClicked()),this,SLOT(actionInvoked()));
    return SnoreBackend::initialize(snore);
}

bool TrayIconNotifer::deinitialize()
{
    if(SnoreBackend::deinitialize())
    {
        if(m_trayIcon)
        {
            disconnect(m_trayIcon,SIGNAL(messageClicked()),this,SLOT(actionInvoked()));
            m_trayIcon = NULL;
        }
        return true;
    }
    return false;
}

void TrayIconNotifer::slotNotify( Notification notification )
{
    m_notificationQue.append(notification);
    if(m_lastNotify.elapsed()> Notification::defaultTimeout() * 1000)
    {
        displayNotification();
    }
}

void TrayIconNotifer::displayNotification()
{
    qDebug()<<"Display"<<m_notificationQue.size();
    Notification notification =  m_notificationQue.takeFirst();
    if(!m_notificationQue.isEmpty())
    {
        QTimer::singleShot(notification.timeout()*1000,this,SLOT(slotCloseNotificationByTimeout()));
    }

    qDebug()<<"taking"<<notification.title();
    m_displayed = notification.id();
    m_trayIcon->showMessage ( Snore::toPlainText(notification.title()),Snore::toPlainText(notification.text()),QSystemTrayIcon::NoIcon,notification.timeout() *1000 );
    m_lastNotify.restart();
}

void TrayIconNotifer::slotCloseNotificationByTimeout()
{
    Notification n = getActiveNotificationByID(m_displayed);
    if(n.isValid())
    {
        closeNotification(n,NotificationEnums::CloseReasons::TIMED_OUT);
    }
    displayNotification();
}

void TrayIconNotifer::actionInvoked()
{
    qDebug()<<"Traicon invoked"<<m_displayed;

    Notification n = getActiveNotificationByID(m_displayed);
    if(n.isValid())
    {
        snore()->d()->notificationActionInvoked(n);
        closeNotification(n,NotificationEnums::CloseReasons::CLOSED);
    }

}

