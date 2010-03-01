#ifndef SNARLNETWORK_H
#define SNARLNETWORK_H
#include "core/interface.h"
#include "parser.h"

struct SnarlNotification{
    class QSharedPointer<class Notification> notification;
    QString action;
    bool httpClient;
    bool vailid;
    class QPointer<class QTcpSocket> clientSocket;
};

class SnarlNetworkFrontend:public QObject,Notification_Frontend{
    Q_OBJECT
    Q_INTERFACES(Notification_Frontend)
    friend class Parser;
public:
    static const int port=9887;

public:
    SnarlNetworkFrontend();
    void actionInvoked(QSharedPointer<Notification>notification);
    void notificationClosed(QSharedPointer<Notification>notification);

private slots:
    void handleConnection();
    void handleMessages();
    void clientDisconnecd();

private:
    class QTcpServer *tcpServer;
    Parser *parser;
    QHash<int,SnarlNotification> notifications;

    void callback(const SnarlNotification &sn,QString msg);

};

#endif //SNARLNETWORK_H
