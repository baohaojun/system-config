#ifndef NOTIFICATION_H
#define NOTIFICATION_H
#include "snore_exports.h"
#include <QString>
#include <QTcpSocket>
#include <QUrl>
#include <QHttp>
#include <QObject>
#include "QHash"
#include <QFile>
#include "application.h"
#include <QPointer>





class SNORE_EXPORT Notification:public QObject
{
    Q_OBJECT
public:    
    Notification();
    Notification(QString source,QString title,QString text,QString icon,int timeout);
    bool isNotification();
    QString toSnalrString()const;

    enum actions{
        TIMED_OUT=0,
        ACTION_1=1,
        ACTION_2=2,
        ACTION_3=3,
        CLOSED=4

    };

    actions actionInvoked;
    QString source;
    QString app;
    QString title;
    QString text;
    QString alert;
    int timeout;      
    void setIcon(const QString &icon){this->icon=icon; }
    QString getIcon();

    QVariantHash hints;

    int id;






private:    
    QString icon;
    bool notification;



};

QDataStream & operator<< ( QDataStream & stream, const Notification & noti);

#endif // NOTIFICATION_H
