#ifndef TRAYICONNOTIFER_H
#define TRAYICONNOTIFER_H

#include "interface.h"
#include "notification/notification.h"

#include <QTime>

class TrayIconNotifer:public Snore::Notification_Backend
{
    Q_OBJECT
    Q_INTERFACES(Snore::Notification_Backend)
public:
    TrayIconNotifer (class QSystemTrayIcon *icon=0 );
    bool isPrimaryNotificationBackend();

public slots:
    void registerApplication ( Snore::Application *application );
    void unregisterApplication ( Snore::Application *application );
    uint notify ( Snore::Notification notification );
    void closeNotification ( Snore::Notification notification );

private:
    class QSystemTrayIcon *_trayIcon;
    QList<Snore::Notification > _notificationQue;
    QTime _lastNotify;
    uint _id;
    uint _displayed;

private slots:
    void displayNotification();
    void actionInvoked();
    void closeNotification();
};

#endif // TRAYICONNOTIFER_H
