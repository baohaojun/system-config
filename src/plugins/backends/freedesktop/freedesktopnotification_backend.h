#ifndef FREEDESKTOPNOTIFICATION_H
#define FREEDESKTOPNOTIFICATION_H
#include "core/plugins/snorebackend.h"
#include <QtDBus>


class fNotification;

class  FreedesktopNotification_Backend:public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES ( Snore::SnoreBackend )
public:
    FreedesktopNotification_Backend ();
    virtual bool init(Snore::SnoreCore *snore);
    
public slots:
    void registerApplication ( Snore::Application *application );
    void unregisterApplication ( Snore::Application *application );
    uint notify ( Snore::Notification notification );
    void closeNotification ( Snore::Notification notification );
    void actionInvoked(const uint &id,const QString &actionID);
    void closed ( const uint &id,const uint &reason );

};
QDBusArgument &operator<<(QDBusArgument &a,const Snore::Notification &i);
const QDBusArgument & operator >>(const QDBusArgument &a,  Snore::Notification &i) ;



#endif // FREEDESKTOPNOTIFICATION_H
