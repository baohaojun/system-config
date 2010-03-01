#ifndef PARSER_H
#define PARSER_H
#include <QString>
#include <QHash>
#include "core/notification.h"
#include <QSharedPointer>
#include <QObject>




class Parser:public QObject{
    Q_OBJECT
public:
    static class QByteArray download(const QUrl &address);

public:
    Parser(class SnarlNetworkFrontend* snarl);

     struct SnarlNotification parse(QString &msg,class QTcpSocket* client);

private:
    class SnarlNetworkFrontend *snarl;
    QString downloadIcon(const QString &address);
    enum snpTypes{
        TYPE,
        APP,
        VERSION,
        ACTION,
        REGISTER,
        ADD_CLASS,
        NOTIFICATION,
        UNREGISTER,
        CLASS,
        TITLE,
        TEXT,
        ICON,
        TIMEOUT,
        ERROR

    };

    QHash<QString,Parser::snpTypes> getSnpType;

};

#endif // PARSER_H
