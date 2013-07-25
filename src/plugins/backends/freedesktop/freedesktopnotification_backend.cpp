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


FreedesktopBackend::FreedesktopBackend () :
    SnoreBackend ( "FreedesktopNotification_Backend",true,true)
{
}

bool FreedesktopBackend::init(SnoreCore *snore){

    m_interface = new org::freedesktop::Notifications( "org.freedesktop.Notifications", "/org/freedesktop/Notifications",
                                                       QDBusConnection::sessionBus(), this );

    QDBusPendingReply<QStringList> reply = m_interface->GetCapabilities();
    reply.waitForFinished();
    QStringList caps  = reply.value();
    m_supportsRichtext = caps.contains( "body-markup" );
    connect(m_interface, SIGNAL(ActionInvoked(uint,QString)), this, SLOT(slotActionInvoked(uint,QString)));
    connect(m_interface, SIGNAL(NotificationClosed(uint,uint)), this , SLOT(slotNotificationClosed(uint,uint)));

    return SnoreBackend::init(snore);
}

void FreedesktopBackend::slotRegisterApplication ( Application *application )
{
    Q_UNUSED ( application );
}

void FreedesktopBackend::slotUnregisterApplication ( Application *application )
{
    Q_UNUSED ( application );
}

void  FreedesktopBackend::slotNotify ( Notification noti )
{    
    QStringList actions;
    foreach(int k,noti.actions().keys())
    {
        actions << QString::number(k) << noti.actions()[k]->name;
    }

    FreedesktopImageHint image(noti.icon().image());

    noti.icon().localUrl();
    QVariantMap hints;
    hints["image_data"] = QVariant::fromValue(image);

    uint updateId = 0;
    if(noti.updateID() != 0)
    {
        updateId = m_snoreIdMap[noti.updateID()];
        m_dbusIdMap[updateId] = noti.id();
        m_snoreIdMap[noti.id()] = updateId;
    }

    QDBusPendingReply<uint>  id = m_interface->Notify(noti.application(), updateId, "", noti.title(),
                                                      noti.text(), actions, hints, noti.sticky()?-1:noti.timeout()*1000);



    if(noti.updateID() == 0)
    {
        id.waitForFinished();
        m_snoreIdMap[noti.id()] = id.value();
        m_dbusIdMap[id.value()] = noti.id();
        qDebug() << "dbus showed " << id.value() << m_dbusIdMap.keys();
    }
}
void FreedesktopBackend::slotActionInvoked(const uint &id, const QString &actionID){
    Notification noti = getActiveNotificationByID(m_dbusIdMap[id]);
    if(!noti.isValid())
        return;
    qDebug() <<"Action"<<id<<"|"<<actionID ;
    noti.setActionInvoked ( actionID.toInt() );
    snore()->notificationActionInvoked ( noti );
}

void FreedesktopBackend::slotCloseNotification ( Notification notification )
{
    uint id = m_snoreIdMap.take(notification.id());
    qDebug() << "dbus closing " << id;
    m_dbusIdMap.remove(id);
    m_interface->CloseNotification(id);
}



void FreedesktopBackend::slotNotificationClosed ( const uint &id,const uint &reason )
{
    NotificationEnums::CloseReasons::closeReasons closeReason = NotificationEnums::CloseReasons::closeReasons(reason);
    qDebug() << "Closed" << id << "|" << closeReason << reason;
    if(id == 0)
        return;
    Notification noti =  getActiveNotificationByID(m_dbusIdMap.take(id));
    m_snoreIdMap.remove(noti.id());

    if(closeReason == NotificationEnums::CloseReasons::CLOSED)
    {
        snore()->notificationActionInvoked(noti);
    }
    closeNotification(noti, closeReason);
}




