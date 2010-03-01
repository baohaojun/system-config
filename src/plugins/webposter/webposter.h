#ifndef WEBPOSTER_H
#define WEBPOSTER_H
#include "core/snore_exports.h"
#include "core/interface.h"
#include <QtNetwork>


class WebPoster: public Notification_Backend{
    Q_OBJECT
    Q_INTERFACES(Notification_Backend)
public:
    WebPoster();
    bool isPrimaryNotificationBackend(){return false;}

public slots:
        int notify(QSharedPointer<Notification>notification);
        void closeNotification(int id);

private:
        QNetworkAccessManager *manager;


};
#endif//WEBPOSTER_H
