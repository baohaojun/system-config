#ifndef SNARL_BACKEND_H
#define SNARL_BACKEND_H
#include "core/interface.h"
#include "SnarlInterface.h"

class Snarl_Backend:public Notification_Backend
{
    Q_OBJECT
    Q_INTERFACES(Notification_Backend)
public:
    Snarl_Backend();
    bool isPrimaryNotificationBackend(){return true;}


protected:
      bool eventFilter(QObject *obj, QEvent *event);
private:   
    Snarl::SnarlInterface *snarlInterface;
public slots:
        int notify(QSharedPointer<Notification>notification);
        void closeNotification(int nr);

};



#endif // SNARL_BACKEND_H
