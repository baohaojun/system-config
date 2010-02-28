#ifndef SNORESERVER_H
#define SNORESERVER_H
#include "snore_exports.h"
#include <QTcpServer>
#include <QTcpSocket>
#include <QObject>
#include <QDir>
#include <QQueue>
#include <QTimer>
#include <QSharedPointer>
#include "application.h"
#include <QSharedPointer>
#include "interface.h"
#include <QDir>


class SNORE_EXPORT SnoreServer:public QObject
{
    Q_OBJECT
public:
    static const QString snoreTMP;


public:
    SnoreServer();
    void publicatePlugin(QObject* plugin);


    int broadcastNotification(QSharedPointer<Notification> notification);
    void closeNotification(QSharedPointer<Notification> notification);
    void notificationActionInvoked(QSharedPointer<Notification> notification);

    void addApplication(QSharedPointer<Application> application);
    bool applicationListAlertIsActive(const QString &applicationName,const QString &alertName);
    void addAlert(const QString &appName,const QString &alertName, const QString &alertTitle);
    void removeApplication(const QString& appName);

    ApplicationsList* getAplicationList(){
        return &applications;
    }

    QHash<QString,QObject*> plugins;

private:
    ApplicationsList applications;


    QList<Notification_Backend*> notyfier;
    Notification_Backend * primaryNotificationBackend;


signals:
    void applicationListChanged();
    void notify(QSharedPointer<Notification> noti);
    void closeNotify(int id);

};


#endif // SNORESERVER_H
