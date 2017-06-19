#include "trayiconnotifer.h"
#include "libsnore/snore.h"
#include "libsnore/snore_p.h"
#include "libsnore/utils.h"

#include <QSystemTrayIcon>
#include <QApplication>

SnorePlugin::Trayicon::Trayicon()
{
    connect(this, &Trayicon::enabledChanged, [this](bool) {
        m_currentlyDisplaying = false;
    });
}

bool SnorePlugin::Trayicon::canCloseNotification() const
{
    return true;
}

void SnorePlugin::Trayicon::slotNotify(Snore::Notification notification)
{
    QSystemTrayIcon *icon = trayIcon(notification.application());
    if (icon) {
        m_notificationQue.append(notification);
        displayNotification(icon);
    } else {
        closeNotification(notification, Snore::Notification::Closed);
        setErrorString(QLatin1String("No tray-icon hint provided for ") + notification.application().name());
    }
}

void SnorePlugin::Trayicon::slotCloseNotification(Snore::Notification n)
{
    QSystemTrayIcon *icon = trayIcon(n.application());
    if (icon) {
        qCDebug(SNORE) << n;
        m_currentlyDisplaying = false;
        displayNotification(icon);
    }
}

void SnorePlugin::Trayicon::slotRegisterApplication(const Snore::Application &application)
{
    QSystemTrayIcon *icon = trayIcon(application);
    if (icon) {
        connect(icon, &QSystemTrayIcon::messageClicked, this, &Trayicon::actionInvoked);
    }
}

void SnorePlugin::Trayicon::slotDeregisterApplication(const Snore::Application &application)
{
    QSystemTrayIcon *icon = trayIcon(application);
    if (icon) {
        disconnect(icon, &QSystemTrayIcon::messageClicked, this, &Trayicon::actionInvoked);
    }
}

QSystemTrayIcon *SnorePlugin::Trayicon::trayIcon(const Snore::Application &app)
{
    if (app.constHints().contains("tray-icon")) {
        return app.constHints().value("tray-icon").value<QPointer<QSystemTrayIcon>>();
    }
    return nullptr;
}

void SnorePlugin::Trayicon::displayNotification(QSystemTrayIcon *icon)
{
    Q_ASSERT(icon);
    if (m_currentlyDisplaying) {
        return;
    }
    if (m_notificationQue.isEmpty()) {
        m_currentlyDisplaying = false;
        return;
    }
    m_currentlyDisplaying = true;
    Snore::Notification notification =  m_notificationQue.takeFirst();
    m_displayed = notification;
    icon->showMessage(notification.title(), notification.text(), QSystemTrayIcon::NoIcon, notification.timeout() * 1000);
    slotNotificationDisplayed(notification);
}

void SnorePlugin::Trayicon::actionInvoked()
{
    Snore::Notification n = m_displayed;
    QSystemTrayIcon *icon = trayIcon(n.application());
    if (icon && n.isValid()) {
        slotNotificationActionInvoked(n);
        closeNotification(n, Snore::Notification::Activated);
        m_currentlyDisplaying = false;
        displayNotification(icon);
    }

}

bool SnorePlugin::Trayicon::isReady()
{
    if (!qobject_cast< QApplication * >(qApp)) {
        setErrorString(tr("This plugin only works with QApplication"));
        return false;
    }
    return true;
}

