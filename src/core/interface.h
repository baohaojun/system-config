#ifndef INTERFACE_H
#define INTERFACE_H
#include "snore_exports.h"
#include "notification.h"
#include <QObject>
#include <QTcpSocket>
#include <QSharedPointer>
#include <QPointer>

class SNORE_EXPORT SnorePlugin{
public:
    virtual ~SnorePlugin(){}; 
    virtual void setSnore(class SnoreServer* snore);
    virtual class SnoreServer* getSnore();
private:
    QPointer<class SnoreServer> snore;

};

class SNORE_EXPORT Notification_Backend:public QObject,public SnorePlugin
{
    Q_OBJECT
public:
    virtual ~Notification_Backend() {}
    virtual bool isPrimaryNotificationBackend()=0;

public slots:
    virtual int notify(QSharedPointer<Notification> notification)=0;
    virtual void closeNotification(int id)=0;

    //    virtual void update

};


class SNORE_EXPORT Notification_Frontend:public SnorePlugin{
public:
    virtual ~Notification_Frontend() {}
     virtual void actionInvoked(QSharedPointer<Notification> notification)=0;
     virtual void notificationClosed(QSharedPointer<Notification> notification)=0;
};



Q_DECLARE_INTERFACE(SnorePlugin,
                    "org.Snore.SnorePlugin/1.0")
Q_DECLARE_INTERFACE(Notification_Frontend,
                    "org.Snore.NotificationFrontend/1.0")
Q_DECLARE_INTERFACE(Notification_Backend,
                    "org.Snore.NotificationBackend/1.0")


#endif//INTERFACE_H
