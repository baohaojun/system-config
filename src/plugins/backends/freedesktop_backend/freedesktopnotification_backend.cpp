#include "freedesktopnotification_backend.h"

#include "libsnore/notification/notification.h"
#include "libsnore/notification/notification_p.h"
#include "libsnore/snore.h"
#include "libsnore/snore_p.h"
#include "libsnore/utils.h"

#include "fredesktopnotification.h"

SnorePlugin::Freedesktop::Freedesktop()
{
    m_interface = new org::freedesktop::Notifications(QStringLiteral("org.freedesktop.Notifications"),
            QStringLiteral("/org/freedesktop/Notifications"),
            QDBusConnection::sessionBus(), this);
    QDBusPendingReply<QStringList> reply = m_interface->GetCapabilities();
    QDBusPendingCallWatcher *watcher = new QDBusPendingCallWatcher(reply, this);
    connect(watcher, &QDBusPendingCallWatcher::finished, [reply, watcher, this]() {
        m_supportsRichtext = reply.value().contains(QStringLiteral("body-markup"));
        watcher->deleteLater();
    });
    connect(this, &Freedesktop::enabledChanged, [this](bool enabled) {
        if (enabled) {
            connect(m_interface, &org::freedesktop::Notifications::ActionInvoked, this, &Freedesktop::slotActionInvoked);
            connect(m_interface, &org::freedesktop::Notifications::NotificationClosed, this , &Freedesktop::slotNotificationClosed);
        } else {
            disconnect(m_interface, &org::freedesktop::Notifications::ActionInvoked, this, &Freedesktop::slotActionInvoked);
            disconnect(m_interface, &org::freedesktop::Notifications::NotificationClosed, this , &Freedesktop::slotNotificationClosed);

        }
    });
}

bool SnorePlugin::Freedesktop::canCloseNotification() const
{
    return true;
}

bool SnorePlugin::Freedesktop::canUpdateNotification() const
{
    return true;
}

void  SnorePlugin::Freedesktop::slotNotify(Snore::Notification noti)
{
    if (noti.data()->sourceAndTargetAreSimilar(this)) {
        return;
    }

    QStringList actions;
    foreach(int k, noti.actions().keys()) {
        actions << QString::number(k) << noti.actions()[k].name();
    }
    QVariantMap hints;

    FreedesktopImageHint image(noti.icon().pixmap(QSize(128, 128)).toImage());
    hints.insert(QStringLiteral("image_data"), QVariant::fromValue(image));

    char urgency = 1;
    if (noti.priority() < 0) {
        urgency = 0;
    } else if (noti.priority() > 2) {
        urgency = 2;
    }
    hints.insert(QStringLiteral("urgency"), urgency);

    if (noti.application().constHints().contains("desktop-entry")) {
        hints.insert(QStringLiteral("desktop-entry"), noti.application().constHints().value("desktop-entry"));
    }

    hints.insert(QStringLiteral("suppress-sound"), noti.constHints().value("silent").toBool());

    uint updateId = 0;
    if (noti.isUpdate()) {
        updateId = noti.old().hints().privateValue(this, "id").toUInt();
        m_dbusIdMap.take(updateId);
    }

    QString title = noti.application().name() + QLatin1String(" - ") + noti.title(m_supportsRichtext ? Snore::Utils::AllMarkup : Snore::Utils::NoMarkup);
    QString body(noti.text(m_supportsRichtext ? Snore::Utils::AllMarkup : Snore::Utils::NoMarkup));
    //TODO: add app icon hint?
    QDBusPendingReply<uint>  id = m_interface->Notify(noti.application().name(), updateId, QString(), title,
                                  body, actions, hints, noti.isSticky() ? -1 : noti.timeout() * 1000);

    id.waitForFinished();
    noti.hints().setPrivateValue(this, "id", id.value());
    m_dbusIdMap[id.value()] = noti;
    slotNotificationDisplayed(noti);

    qCDebug(SNORE) << noti.id() << "|" << id.value();
}

void SnorePlugin::Freedesktop::slotActionInvoked(const uint id, const QString &actionID)
{
    qCDebug(SNORE) << id << m_dbusIdMap[id];
    Snore::Notification noti = m_dbusIdMap[id];
    if (!noti.isValid()) {
        return;
    }
    slotNotificationActionInvoked(noti, noti.actions().value(actionID.toInt()));;
}

void SnorePlugin::Freedesktop::slotCloseNotification(Snore::Notification notification)
{
    uint id = notification.hints().privateValue(this, "id").toUInt();
    qCDebug(SNORE) << notification.id() << id;
    m_interface->CloseNotification(id);
}

void SnorePlugin::Freedesktop::slotNotificationClosed(const uint id, const uint reason)
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
    Snore::Notification::CloseReasons closeReason;
    switch (reason) {
    case (1):
        closeReason = Snore::Notification::TimedOut;
        break;
    case (2):
        closeReason = Snore::Notification::Dismissed;
        break;
    case (3):
        closeReason = Snore::Notification::Closed;
        break;
    default:
        closeReason = Snore::Notification::None;
    }

    qCDebug(SNORE) << id << "|" << closeReason << reason;
    if (id == 0) {
        return;
    }
    Snore::Notification noti =  m_dbusIdMap.take(id);
    if (noti.isValid()) {
        closeNotification(noti, closeReason);
    }
}

