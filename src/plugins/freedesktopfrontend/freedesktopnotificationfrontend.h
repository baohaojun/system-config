#ifndef FREEDESKTOPNOTIFICATION_FRONTEND_H
#define FREEDESKTOPNOTIFICATION_FRONTEND_H
#include "core/interface.h"
#include <QtDBus>

class FreedesktopNotification_Frontend:public QObject,Notification_Frontend{
    Q_OBJECT
    Q_INTERFACES(Notification_Frontend)
public:
    FreedesktopNotification_Frontend();
    ~FreedesktopNotification_Frontend();

    QString getImagefromHint(const class FreedesktopImageHint &img);

    void actionInvoked(QSharedPointer<Notification>notification);
    void notificationClosed(QSharedPointer<Notification>notification);

    uint Notify(const QString &app_name, uint replaces_id, const QString &app_icon, const QString &summary, const QString &body, const QStringList &actions, const QVariantMap &hints, int timeout);

    void CloseNotification( uint id );

    QStringList GetCapabilities();

    QString GetServerInformation(QString& vendor, QString& version, QString& specVersion);

signals:
    void NotificationClosed( uint id, uint reason );
    void ActionInvoked( uint id, const QString& actionKey );



};

#endif//FREEDESKTOPNOTIFICATION_FRONTEND_H
