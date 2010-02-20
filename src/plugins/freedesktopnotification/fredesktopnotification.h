#ifndef FreedesktopNotification_H
#define FreedesktopNotification_H
#include <QtDBus>
#include <QPointer>
#include "core/notification.h"
#include <QMetaType>


class FreedesktopImageHint;

class FreedesktopNotification{
public:
    static void registerTypes();

public:
    FreedesktopNotification();
    FreedesktopNotification(Notification *noti);

    QPointer<FreedesktopImageHint> image;
    Notification toNotification();
    QPointer<Notification> notification;

};

Q_DECLARE_METATYPE(FreedesktopNotification);

QDBusArgument &operator<<(QDBusArgument &a, const FreedesktopNotification &i);
const QDBusArgument & operator >>(const QDBusArgument &a,  FreedesktopNotification &i) ;

class FreedesktopImageHint
{
public:
    FreedesktopImageHint();
    FreedesktopImageHint(const QImage &img);

    QImage toQImage()const;

    int width;
    int height;
    int rowstride;
    bool hasAlpha;
    int bitsPerSample;
    int channels;
    QByteArray imageData;


};
Q_DECLARE_METATYPE(FreedesktopImageHint);

QDBusArgument &operator<<(QDBusArgument &a, const FreedesktopImageHint &i);
const QDBusArgument & operator >>(const QDBusArgument &a,  FreedesktopImageHint  &i) ;

#endif // FreedesktopNotification_H
