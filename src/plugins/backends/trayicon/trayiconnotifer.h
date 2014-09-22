#ifndef TRAYICONNOTIFER_H
#define TRAYICONNOTIFER_H

#include "core/plugins/snorebackend.h"

namespace Snore
{
class SnoreCore;
}

class QSystemTrayIcon;

class TrayIconNotifer: public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationBackend/1.0" FILE "plugin.json")
public:
    TrayIconNotifer();
    virtual ~TrayIconNotifer();
    virtual bool initialize(Snore::SnoreCore *snore);
    virtual bool deinitialize();

public slots:
    void slotNotify(Snore::Notification notification);
    void slotCloseNotification(Snore::Notification notification);

private:
    QSystemTrayIcon *m_trayIcon;
    QList<Snore::Notification > m_notificationQue;
    uint m_displayed;
    bool m_currentlyDisplaying;

private slots:
    void displayNotification();
    void actionInvoked();
};

#endif // TRAYICONNOTIFER_H
