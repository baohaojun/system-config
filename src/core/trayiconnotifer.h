#ifndef TRAYICONNOTIFER_H
#define TRAYICONNOTIFER_H

#include "interface.h"
#include "notification.h"

class TrayIconNotifer:public Notification_Backend
{
    Q_OBJECT
public:
    TrayIconNotifer ( class SnoreServer *snore=0,class QSystemTrayIcon *icon=0 );
    bool isPrimaryNotificationBackend();

public slots:
    void registerApplication ( Application *application );
    void unregisterApplication ( class Application *application );
    int notify ( QSharedPointer<Notification> notification );
    void closeNotification ( QSharedPointer<Notification> notification );

private:
    class QSystemTrayIcon *_trayIcon;
    int _id;
};

#endif // TRAYICONNOTIFER_H
