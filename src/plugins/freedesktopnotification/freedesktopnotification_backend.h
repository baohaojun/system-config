#ifndef FREEDESKTOPNOTIFICATION_H
#define FREEDESKTOPNOTIFICATION_H
#include "core/interface.h"
#include <QtDBus>


class fNotification;

class  FreedesktopNotification_Backend:public Notification_Backend
{
    Q_OBJECT
    Q_INTERFACES ( Notification_Backend )
public:
    FreedesktopNotification_Backend ( class SnoreServer *snore=0 );
    bool isPrimaryNotificationBackend();
    
public slots:
    void registerApplication ( Application *application );
    void unregisterApplication ( Application *application );
    uint notify ( Notification notification );
    void closeNotification ( Notification notification );
    void actionInvoked(const uint &id,const QString &actionID);
    void closed ( const uint &id,const uint &reason );

};
QDBusArgument &operator<<(QDBusArgument &a,const Notification &i);
const QDBusArgument & operator >>(const QDBusArgument &a,  Notification &i) ;



#endif // FREEDESKTOPNOTIFICATION_H
