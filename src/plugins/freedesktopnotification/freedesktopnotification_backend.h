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
    int notify ( QSharedPointer<class Notification>notification );
    void closeNotification ( QSharedPointer<Notification> notification );

};




class  fNotification:public QObject
{
    Q_OBJECT
    friend class FreedesktopNotification_Backend;
public:
    static QDBusInterface notificationInterface;
private:
    static QString vendor;

public:
    fNotification ( QSharedPointer<Notification> notification);
    uint send();
    QSharedPointer<Notification> notification();


private:
    Notification_Backend* parent;
    QString getVendor();
    QTimer *selfdistruct;
    QSharedPointer<Notification> _notification;

private slots:
    void action ( const uint &id, const QString &action_key );
    void closed ( const uint &id,const uint &reason );
    void close();
    void selfDelete();
};


#endif // FREEDESKTOPNOTIFICATION_H
