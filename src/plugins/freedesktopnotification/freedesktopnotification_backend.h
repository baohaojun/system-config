#ifndef FREEDESKTOPNOTIFICATION_H
#define FREEDESKTOPNOTIFICATION_H
#include "core/interface.h"
#include <QtDBus>


class fNotification;

class  FreedesktopNotification_Backend:public Notification_Backend
{
    Q_OBJECT
    Q_INTERFACES(Notification_Backend)
public:            
    FreedesktopNotification_Backend()
    {
        setProperty("name","FreedesktopNotification_Backend");
    };
    bool isPrimaryNotificationBackend(){return true;}    
public slots:
    int notify(QSharedPointer<class Notification>notification);
    void closeNotification(int id);

};




class  fNotification:public QObject{
    Q_OBJECT
public:
    static QDBusInterface notificationInterface;
private:
    static QString vendor;

public:
    fNotification(FreedesktopNotification_Backend* parent);
    int send();
    QSharedPointer<Notification> notification;

private:
    Notification_Backend* parent;
    QString getVendor();
    QTimer selfdistruct;

private slots:
    void action(const uint &id, const QString &action_key);
    void closed(const uint &id,const uint &reason);
    void close();
    void selfDelete();
};


#endif // FREEDESKTOPNOTIFICATION_H
