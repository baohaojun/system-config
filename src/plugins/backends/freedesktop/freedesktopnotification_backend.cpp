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
    QStringList caps  = reply.reply().arguments().first().toStringList();
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

    QVariantMap hints;
    hints["image_data"] = QVariant::fromValue(FreedesktopImageHint(noti.icon().image().scaledToWidth(50,Qt::FastTransformation)));

    uint updateId = 0;
    if(noti.updateID() != 0)
    {
        updateId = m_idMap[noti.updateID()];
        noti.hints().setValue("DBUS_ID", updateId);
    }

    QDBusPendingReply<uint>  id = m_interface->Notify(noti.application(), updateId, "", noti.title(),
                                                      noti.text(), actions, hints, noti.timeout()*1000);



    if(noti.updateID() == 0)
    {
        id.waitForFinished();
        noti.hints().setValue("DBUS_ID", id.value());
        m_idMap[id.value()] = noti.id();
    }
}
void FreedesktopBackend::slotActionInvoked(const uint &id, const QString &actionID){
    Notification noti = getActiveNotificationByID(m_idMap[id]);
    if(!noti.isValid())
        return;
    qDebug() <<"Action"<<id<<"|"<<actionID ;
    noti.setActionInvoked ( actionID.toInt() );
    snore()->notificationActionInvoked ( noti );
}

void FreedesktopBackend::slotCloseNotification ( Notification notification )
{
    uint id = notification.hints().value("DBUS_ID").toUInt();
    m_idMap.remove(id);
    m_interface->CloseNotification(id);
}



void FreedesktopBackend::slotNotificationClosed ( const uint &id,const uint &reason )
{
    qDebug() <<"Closed"<<id<<"|"<<reason;
    if(id == 0)
        return;
    Notification noti =  getActiveNotificationByID(m_idMap.take(id));
    closeNotification(noti ,NotificationEnums::CloseReasons::closeReasons(reason));
}




