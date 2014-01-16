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
    SnoreBackend ( "SystemTray",true,false ),
    m_trayIcon(NULL),
    m_displayed(-1),
    m_currentlyDisplaying(false)
{

}

TrayIconNotifer::~TrayIconNotifer()
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
            m_currentlyDisplaying = false;
        }
        return true;
    }
    return false;
}

void TrayIconNotifer::slotNotify( Notification notification )
{
    m_notificationQue.append(notification);
    displayNotification();
}

void TrayIconNotifer::slotCloseNotification(Notification n)
{
    qDebug() << Q_FUNC_INFO << n;
    m_currentlyDisplaying = false;
    displayNotification();
}

void TrayIconNotifer::displayNotification()
{
    if(m_currentlyDisplaying)
    {
        return;
    }
    if(m_notificationQue.isEmpty())
    {
        m_currentlyDisplaying = false;
        return;
    }
    m_currentlyDisplaying = true;
    Notification notification =  m_notificationQue.takeFirst();
    m_displayed = notification.id();
    m_trayIcon->showMessage ( Snore::toPlainText(notification.title()),Snore::toPlainText(notification.text()),QSystemTrayIcon::NoIcon,notification.timeout() *1000 );
   startTimeout(notification.id(),notification.timeout());
}

void TrayIconNotifer::actionInvoked()
{
    Notification n = getActiveNotificationByID(m_displayed);
    if(n.isValid())
    {
        snore()->d()->notificationActionInvoked(n);
        closeNotification(n,NotificationEnums::CloseReasons::CLOSED);
        m_currentlyDisplaying = false;
        displayNotification();
    }

}

