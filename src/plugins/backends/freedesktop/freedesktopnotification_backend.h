#ifndef FREEDESKTOPNOTIFICATION_H
#define FREEDESKTOPNOTIFICATION_H
#include "core/plugins/snorebackend.h"
#include "notificationinterface.h"

class fNotification;

class  FreedesktopBackend:public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES ( Snore::SnoreBackend )
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationBackend/1.0")
public:
    FreedesktopBackend();
    virtual bool init(Snore::SnoreCore *snore);
    
public slots:
    void slotNotify( Snore::Notification notification );
    void slotCloseNotification ( Snore::Notification notification );

    void slotActionInvoked(const uint &id,const QString &actionID);
    void slotNotificationClosed ( const uint &id,const uint &reason );
    void slotRegisterApplication ( Snore::Application *application );
    void slotUnregisterApplication ( Snore::Application *application );


private:
    org::freedesktop::Notifications* m_interface;
    QHash<uint,uint> m_idMap;

};
QDBusArgument &operator<<(QDBusArgument &a,const Snore::Notification &i);
const QDBusArgument & operator >>(const QDBusArgument &a,  Snore::Notification &i) ;



#endif // FREEDESKTOPNOTIFICATION_H
