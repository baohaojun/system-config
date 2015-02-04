#include "trayiconnotifer.h"
#include "core/snore.h"
#include "core/snore_p.h"

#include <QSystemTrayIcon>
using namespace Snore;

TrayIconNotifer::TrayIconNotifer() :
    SnoreBackend("System Tray Icon", true, false),
    m_displayed(-1),
    m_currentlyDisplaying(false)
{

}

TrayIconNotifer::~TrayIconNotifer()
{

}

bool TrayIconNotifer::deinitialize()
{
    if (SnoreBackend::deinitialize()) {
        m_currentlyDisplaying = false;
        return true;
    }
    return false;
}

void TrayIconNotifer::slotNotify(Notification notification)
{
    QSystemTrayIcon *icon = trayIcon(notification.application());
    if (icon) {
        m_notificationQue.append(notification);
        displayNotification(icon);
    }
}

void TrayIconNotifer::slotCloseNotification(Notification n)
{
    QSystemTrayIcon *icon = trayIcon(n.application());
    if (icon) {
        snoreDebug(SNORE_DEBUG) << n;
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
        return app.constHints().value("tray-icon").value<QSystemTrayIcon *>();
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
    m_displayed = notification.id();
    icon->showMessage(Snore::toPlainText(notification.title()), Snore::toPlainText(notification.text()), QSystemTrayIcon::NoIcon, notification.timeout() * 1000);
    startTimeout(notification);
}

void TrayIconNotifer::actionInvoked()
{
    Notification n = getActiveNotificationByID(m_displayed);
    QSystemTrayIcon *icon = trayIcon(n.application());
    if (icon && n.isValid()) {
        SnoreCorePrivate::instance()->notificationActionInvoked(n);
        closeNotification(n, Notification::CLOSED);
        m_currentlyDisplaying = false;
        displayNotification(icon);
    }

}

