#ifndef TRAYICONNOTIFER_H
#define TRAYICONNOTIFER_H

#include "libsnore/plugins/snorebackend.h"

class QSystemTrayIcon;

namespace SnorePlugin {

class Trayicon: public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationBackend/1.0" FILE "snore_plugin.json")
public:
    Trayicon();
    ~Trayicon() = default;

    bool canCloseNotification() const override;
    bool isReady() override;

public Q_SLOTS:
    void slotNotify(Snore::Notification notification) override;
    void slotCloseNotification(Snore::Notification notification) override;
    void slotRegisterApplication(const Snore::Application &application) override;
    void slotDeregisterApplication(const Snore::Application &application) override;

private:
    QSystemTrayIcon *trayIcon(const Snore::Application &app);
    QList<Snore::Notification > m_notificationQue;
    Snore::Notification m_displayed;
    bool m_currentlyDisplaying = false;

private Q_SLOTS:
    void displayNotification(QSystemTrayIcon *icon);
    void actionInvoked();

};
}

#endif // TRAYICONNOTIFER_H
