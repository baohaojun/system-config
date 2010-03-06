#ifndef GROWL_BACKEND_H
#define GROWL_BACKEND_H
#include "core/interface.h"
class Growl_Backend:public Notification_Backend
{
    Q_OBJECT
    Q_INTERFACES(Notification_Backend);
public:
    Growl_Backend();
    ~Growl_Backend();
    bool isPrimaryNotificationBackend(){return true;}
private:
    uint id;
    class Growl *growl;
public slots:
    int notify(QSharedPointer<Notification>notification);
    void closeNotification(int nr);
};

#endif // GROWL_BACKEND_H
