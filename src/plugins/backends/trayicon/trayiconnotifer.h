#ifndef TRAYICONNOTIFER_H
#define TRAYICONNOTIFER_H

#include "core/plugins/snorebackend.h"

#include <QTime>

namespace Snore{
    class SnoreCore;
}

class QSystemTrayIcon;

class TrayIconNotifer:public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
public:
    TrayIconNotifer ();
    virtual bool init(Snore::SnoreCore *snore);
    bool isPrimaryNotificationBackend();

public slots:
    void registerApplication ( Snore::Application *application );
    void unregisterApplication ( Snore::Application *application );
    uint notify ( Snore::Notification notification );
    void closeNotification ( Snore::Notification notification );

private:
    QSystemTrayIcon *m_trayIcon;
    QList<Snore::Notification > m_notificationQue;
    QTime m_lastNotify;
    uint m_id;
    uint m_displayed;

private slots:
    void displayNotification();
    void actionInvoked();
    void closeNotification();
};

#endif // TRAYICONNOTIFER_H
