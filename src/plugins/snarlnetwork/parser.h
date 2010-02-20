#ifndef PARSER_H
#define PARSER_H
#include <QString>
#include <QHash>
#include "core/notification.h"
#include <QSharedPointer>
#include <QObject>




class Parser{
public:
    Parser(class SnarlNetworkFrontend* snarl);

     struct SnarlNotification parse(QString &msg,QTcpSocket* client);

private:
    class SnarlNetworkFrontend *snarl;
    QString download(const QString &address);
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
