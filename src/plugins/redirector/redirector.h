#ifndef REDIRECTOR_H
#define REDIRECTOR_H
#include "core/interface.h"
#include <QHash>
#include "webinterface/webinterface.h"

class Redircetor:public Notification_Backend,WebInterface_Plugin{
    Q_OBJECT
    Q_INTERFACES(Notification_Backend WebInterface_Plugin)
public:
    static const int port=9887;
public:
    Redircetor();
    bool isPrimaryNotificationBackend(){return false;}
    QString display();
    bool parseCommand(QTcpSocket *client, const QString &command);
    class SnoreServer* getSnore();
    void setSnore(class SnoreServer *snore);

public slots:
        int notify(QSharedPointer<class Notification>notification);
        void closeNotification(int nr);

private:
    QHash<QString,QSharedPointer<QTcpSocket> > subscribers;
    QPointer<class SnoreServer> snore;

    enum ARGUMENTS{
        SUBSCRIBE=1,
        UNSUBSCRIBE=2,
        LISTSUBSCRIBERS=3,
    };
    QHash<QString,Redircetor::ARGUMENTS> getArgument;

};


#endif//REDIRECTOR_H
