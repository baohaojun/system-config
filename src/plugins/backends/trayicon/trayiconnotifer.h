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

public slots:
    void slotRegisterApplication ( Snore::Application *application );
    void slotUnregisterApplication ( Snore::Application *application );
    void slotNotify ( Snore::Notification notification );
    bool slotCloseNotification ( Snore::Notification notification );

private:
    QSystemTrayIcon *m_trayIcon;
    QList<Snore::Notification > m_notificationQue;
    QTime m_lastNotify;
    uint m_displayed;

private slots:
    void displayNotification();
    void actionInvoked();
    void slotCloseNotification();
};

#endif // TRAYICONNOTIFER_H
