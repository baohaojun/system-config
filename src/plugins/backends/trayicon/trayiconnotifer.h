#ifndef TRAYICONNOTIFER_H
#define TRAYICONNOTIFER_H

#include "libsnore/plugins/snorebackend.h"

namespace Snore
{
}

class QSystemTrayIcon;

class TrayIconNotifer: public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationBackend/1.0" FILE "plugin.json")
public:
    TrayIconNotifer() = default;
    ~TrayIconNotifer() = default;
    virtual bool deinitialize() override;

    virtual bool canCloseNotification() const override;

public Q_SLOTS:
    void slotNotify(Snore::Notification notification);
    void slotCloseNotification(Snore::Notification notification);
    void slotRegisterApplication(const Snore::Application &application);
    void slotDeregisterApplication(const Snore::Application &application);

private:
    QSystemTrayIcon *trayIcon(const Snore::Application &app);
    QList<Snore::Notification > m_notificationQue;
    Snore::Notification m_displayed;
    bool m_currentlyDisplaying = false;

private Q_SLOTS:
    void displayNotification(QSystemTrayIcon *icon);
    void actionInvoked();

};

#endif // TRAYICONNOTIFER_H
