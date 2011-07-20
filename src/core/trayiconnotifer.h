#ifndef TRAYICONNOTIFER_H
#define TRAYICONNOTIFER_H

#include "interface.h"
#include "notification/notification.h"

class TrayIconNotifer:public Notification_Backend
{
    Q_OBJECT
    Q_INTERFACES(Notification_Backend)
public:
            TrayIconNotifer ( class SnoreServer *snore=0,class QSystemTrayIcon *icon=0 );
    bool isPrimaryNotificationBackend();

public slots:
    void registerApplication ( Application *application );
    void unregisterApplication ( class Application *application );
    int notify ( Notification notification );
    void closeNotification ( Notification notification );

private:
    class QSystemTrayIcon *_trayIcon;
    QList<Notification > _notificationQue;
    bool _noNotificationDisplayed;
    int _id;

private slots:
    void displayNotification();
};

#endif // TRAYICONNOTIFER_H
