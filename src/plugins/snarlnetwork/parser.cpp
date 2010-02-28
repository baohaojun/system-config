#include "parser.h"
#include "core/snoreserver.h"
#include "snarlnetwork.h"
#include "core/notification.h"
#include <QDir>
#include <QCryptographicHash>
#include <QNetworkAccessManager>
#include <QEventLoop>
#include <QNetworkReply>
#include <QObject>
#include <QTcpSocket>

Parser::Parser(SnarlNetworkFrontend *snarl):snarl(snarl)
{
    getSnpType.insert("type",TYPE);
    getSnpType.insert("app",APP);
    getSnpType.insert("version",VERSION);
    getSnpType.insert("action",ACTION);
    getSnpType.insert("register",REGISTER);
    getSnpType.insert("add_class",ADD_CLASS);
    getSnpType.insert("notification",NOTIFICATION);
    getSnpType.insert("unregister",UNREGISTER);
    getSnpType.insert("class",CLASS);
    getSnpType.insert("title",TITLE);
    getSnpType.insert("text",TEXT);
    getSnpType.insert("icon",ICON);
    getSnpType.insert("timeout",TIMEOUT);
}


SnarlNotification Parser::parse(QString &msg,QTcpSocket* client){
    msg=msg.trimmed();

    SnarlNotification sNotification;
    sNotification.httpClient=false;
    sNotification.vailid=true;
    sNotification.notification=QSharedPointer<Notification>(new Notification());
    sNotification.clientSocket=client;
    sNotification.notification->setIsNotification(false);

    snpTypes action(ERROR);  
    if(msg.startsWith("GET ")){
        msg=msg.mid(msg.indexOf("/")+1);
        msg=msg.mid(0,msg.indexOf(" "));
        QByteArray dat(QByteArray::fromBase64(msg.toLatin1().data()));
        msg=QString(dat);
        qDebug()<<"Notification from a browser"<<msg;
        sNotification.httpClient=true;
    }


    QString key;
    QString value;
    QStringList splitted=msg.split("#?");
    foreach(QString s,splitted){
        key=s.mid(0,s.indexOf("=")).toLower();
        value=s.mid(s.indexOf("=")+1);
        switch(getSnpType.value(key)){
        case APP:
            sNotification.notification->app=value;
            break;
        case ACTION:
            action=getSnpType.value(value);
            sNotification.action=value;
            break;
        case  TITLE:
            sNotification.notification->title=value;
            break;
        case TEXT:
            sNotification.notification->text=value;
            break;
        case ICON:
            sNotification.notification->setIcon(downloadIcon(value));
            break;
        case CLASS:
            sNotification.notification->alert=value;
        case TIMEOUT:
            sNotification.notification->timeout=value.toInt();
            break;
        default:
            break;
        }
    }


    switch(action){
    case NOTIFICATION:
        if(snarl->getSnore()->applicationListAlertIsActive(sNotification.notification->app,sNotification.notification->alert))
            break;
        sNotification.notification->setIsNotification(true);
        return sNotification;
        break;    
    case ADD_CLASS:
        if(sNotification.notification->alert.isEmpty()){
            qDebug()<<"Error registering alert with empty name";
            break;
        }
        if(sNotification.notification->title.isEmpty())
            sNotification.notification->title = sNotification.notification->alert;
        snarl->getSnore()->addAlert(sNotification.notification->app,sNotification.notification->alert,sNotification.notification->title);
        break;
    case REGISTER:
        qDebug()<<snarl->getSnore()->getAplicationList()->keys();
        if(!snarl->getSnore()->getAplicationList()->contains(sNotification.notification->app)&&!sNotification.notification->app.isEmpty()){
            snarl->getSnore()->addApplication(QSharedPointer<Application>(new Application(sNotification.notification->app)));
        }
        else
            qDebug()<<sNotification.notification->app<<"already registred";
        break;
    case UNREGISTER:
        snarl->getSnore()->removeApplication(sNotification.notification->app);
        break;
    case ERROR:
    default:
        sNotification.vailid=false;
        break;
    }
    qDebug()<<sNotification.notification->toSnalrString();
    sNotification.notification->hints.insert("SNaction",sNotification.action);
    return sNotification;
}

QString Parser::downloadIcon(const QString &address){
    if(address=="")
        return "";
    if(address.startsWith("file://"))
        return QString(address.mid(7));
    QByteArray arr=address.toUtf8();
    QUrl url=QUrl::fromEncoded(arr);

    QCryptographicHash hash(QCryptographicHash::Md5);
    hash.addData(arr);
    QString filename=QDir::temp().path()+"/SnoreNotify/"+hash.result().toHex()+address.mid(address.lastIndexOf(".")-1);
    QFile file(filename);
    if(file.exists())
        return filename;

    QByteArray reply=download(url);

    file.open(QIODevice::WriteOnly);
    file.write(reply);

    return filename;

}

QByteArray Parser::download(const QUrl &address){
    QNetworkAccessManager manager;
    QEventLoop loop;
    QNetworkRequest request(address);
    request.setRawHeader("User-Agent", "SnoreNotify");
    QNetworkReply *reply=manager.get(request);
    QObject::connect(reply, SIGNAL(finished()), &loop, SLOT(quit()));
    loop.exec();
    return reply->readAll();
}

//#include "parser.moc"
