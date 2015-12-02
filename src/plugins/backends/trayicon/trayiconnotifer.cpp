#include "trayiconnotifer.h"
#include "libsnore/snore.h"
#include "libsnore/snore_p.h"
#include "libsnore/utils.h"

#include <QSystemTrayIcon>
#include <QApplication>

using namespace Snore;

TrayIconNotifer::TrayIconNotifer()
{
    connect(this, &TrayIconNotifer::enabledChanged, [this](bool) {
        m_currentlyDisplaying = false;
    });
}

bool TrayIconNotifer::canCloseNotification() const
{
    return true;
}

void TrayIconNotifer::slotNotify(Notification notification)
{
    QSystemTrayIcon *icon = trayIcon(notification.application());
    if (icon) {
        m_notificationQue.append(notification);
        displayNotification(icon);
    } else {
        closeNotification(notification, Notification::CLOSED);
        setErrorString(QLatin1String("No tray-icon hint provided for ") + notification.application().name());
    }
}

void TrayIconNotifer::slotCloseNotification(Notification n)
{
    QSystemTrayIcon *icon = trayIcon(n.application());
    if (icon) {
        qCDebug(SNORE) << n;
        m_currentlyDisplaying = false;
        displayNotification(icon);
    }
}

void TrayIconNotifer::slotRegisterApplication(const Application &application)
{
    QSystemTrayIcon *icon = trayIcon(application);
    if (icon) {
        connect(icon, SIGNAL(messageClicked()), this, SLOT(actionInvoked()));
    }
}

void TrayIconNotifer::slotDeregisterApplication(const Application &application)
{
    QSystemTrayIcon *icon = trayIcon(application);
    if (icon) {
        disconnect(icon, SIGNAL(messageClicked()), this, SLOT(actionInvoked()));
    }
}

QSystemTrayIcon *TrayIconNotifer::trayIcon(const Application &app)
{
    if (app.constHints().contains("tray-icon")) {
        return app.constHints().value("tray-icon").value<QPointer<QSystemTrayIcon>>();
    }
    return nullptr;
}

void TrayIconNotifer::displayNotification(QSystemTrayIcon *icon)
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
    Notification notification =  m_notificationQue.takeFirst();
    m_displayed = notification;
    icon->showMessage(notification.title(), notification.text(), QSystemTrayIcon::NoIcon, notification.timeout() * 1000);
    slotNotificationDisplayed(notification);
}

void TrayIconNotifer::actionInvoked()
{
    Notification n = m_displayed;
    QSystemTrayIcon *icon = trayIcon(n.application());
    if (icon && n.isValid()) {
        slotNotificationActionInvoked(n);
        closeNotification(n, Notification::ACTIVATED);
        m_currentlyDisplaying = false;
        displayNotification(icon);
    }

}

bool TrayIconNotifer::isReady()
{    
    if(!qobject_cast< QApplication* >(qApp)){
     setErrorString(tr("This plugin only works with QApllication"));
     return false;
    }
    return true;
}

