#include "freedesktopnotification_backend.h"

#include "libsnore/notification/notification.h"
#include "libsnore/notification/notification_p.h"
#include "libsnore/snore.h"
#include "libsnore/snore_p.h"
#include "libsnore/utils.h"

#include "fredesktopnotification.h"

using namespace Snore;

bool FreedesktopBackend::initialize()
{

    m_interface = new org::freedesktop::Notifications(QLatin1String("org.freedesktop.Notifications"),
            QLatin1String("/org/freedesktop/Notifications"),
            QDBusConnection::sessionBus(), this);

    QDBusPendingReply<QStringList> reply = m_interface->GetCapabilities();
    reply.waitForFinished();
    QStringList caps  = reply.value();
    m_supportsRichtext = caps.contains(QLatin1String("body-markup"));
    connect(m_interface, SIGNAL(ActionInvoked(uint,QString)), this, SLOT(slotActionInvoked(uint,QString)));
    connect(m_interface, SIGNAL(NotificationClosed(uint,uint)), this , SLOT(slotNotificationClosed(uint,uint)));

    return SnoreBackend::initialize();
}

bool FreedesktopBackend::deinitialize()
{
    if (SnoreBackend::deinitialize()) {
        disconnect(m_interface, SIGNAL(ActionInvoked(uint,QString)), this, SLOT(slotActionInvoked(uint,QString)));
        disconnect(m_interface, SIGNAL(NotificationClosed(uint,uint)), this , SLOT(slotNotificationClosed(uint,uint)));
        m_interface->deleteLater();
        m_interface = nullptr;
        m_dbusIdMap.clear();
        return true;
    }
    return false;
}

bool FreedesktopBackend::canCloseNotification() const
{
    return true;
}

bool FreedesktopBackend::canUpdateNotification() const
{
    return true;
}

void  FreedesktopBackend::slotNotify(Notification noti)
{
    if(noti.data()->source()->name() == name()) {
        return;
    }

    QStringList actions;
    foreach (int k, noti.actions().keys()) {
        actions << QString::number(k) << noti.actions()[k].name();
    }
    QVariantMap hints;
    if (noti.icon().isValid()) {
        FreedesktopImageHint image(noti.icon().image());
        hints.insert(QLatin1String("image_data"), QVariant::fromValue(image));
    }

    char urgency = '1';
    if (noti.priority() < 0) {
        urgency = '0';
    } else if (noti.priority() > 2) {
        urgency = '2';
    }
    hints.insert(QLatin1String("urgency"), urgency);

    if (noti.application().constHints().contains("desktop-entry")) {
        hints.insert(QLatin1String("desktop-entry"), noti.application().constHints().value("desktop-entry"));
    }

    uint updateId = 0;
    if (noti.isUpdate()) {
        updateId = noti.old().hints().privateValue(this, "id").toUInt();
        m_dbusIdMap.take(updateId);
    }

    QString title = noti.application().name() + QLatin1String(" - ") + noti.title(m_supportsRichtext ? Utils::ALL_MARKUP : Utils::NO_MARKUP);
    QString body(noti.text(m_supportsRichtext ? Utils::ALL_MARKUP : Utils::NO_MARKUP));
    //TODO: add app icon hint?
    QDBusPendingReply<uint>  id = m_interface->Notify(noti.application().name(), updateId, QString(), title,
                                  body, actions, hints, noti.isSticky() ? -1 : noti.timeout() * 1000);

    id.waitForFinished();
    noti.hints().setPrivateValue(this, "id", id.value());
    m_dbusIdMap[id.value()] = noti;
    slotNotificationDisplayed(noti);

    snoreDebug(SNORE_DEBUG) << noti.id() << "|" << id.value();
}

void FreedesktopBackend::slotActionInvoked(const uint &id, const QString &actionID)
{
    snoreDebug(SNORE_DEBUG) << id << m_dbusIdMap[id];
    Notification noti = m_dbusIdMap[id];
    if (!noti.isValid()) {
        return;
    }
    slotNotificationActionInvoked(noti, noti.actions().value(actionID.toInt()));;
}

void FreedesktopBackend::slotCloseNotification(Notification notification)
{
    uint id = notification.hints().privateValue(this, "id").toUInt();
    snoreDebug(SNORE_DEBUG) << notification.id() << id;
    m_interface->CloseNotification(id);
}

void FreedesktopBackend::slotNotificationClosed(const uint &id, const uint &reason)
{
    /*
     *
     *  The reason the notification was closed.
     *
     *  1 - The notification expired.
     *
     *  2 - The notification was dismissed by the user.
     *
     *  3 - The notification was closed by a call to CloseNotification.
     *
     *  4 - Undefined/reserved reasons.
    */
    Notification::CloseReasons closeReason;
    switch (reason) {
    case (1):
        closeReason = Notification::TIMED_OUT;
        break;
    case (2):
        closeReason = Notification::DISMISSED;
        break;
    case (3):
        closeReason = Notification::CLOSED;
        break;
    default:
        closeReason = Notification::NONE;
    }

    snoreDebug(SNORE_DEBUG) << id << "|" << closeReason << reason;
    if (id == 0) {
        return;
    }
    Notification noti =  m_dbusIdMap.take(id);
    if (noti.isValid()) {
        closeNotification(noti, closeReason);
    }
}

