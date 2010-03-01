#include "webposter.h"
#include <QDebug>
#include <QtCore>
#include <iostream>
Q_EXPORT_PLUGIN2(webposter,WebPoster)

WebPoster::WebPoster(){
    setProperty("name","WebPoster");
    manager=new QNetworkAccessManager(this);
}

int WebPoster::notify(QSharedPointer<Notification>notification){
    QByteArray byte(notification->toSnalrString().toLatin1().data());
    QUrl url("http://www.pro-zeit.ch/index.php");
    url.addEncodedQueryItem("action","add");
    url.addEncodedQueryItem("data",QUrl::toPercentEncoding(byte.toBase64()));
    QNetworkRequest request(url);
    request.setRawHeader("User-Agent", "SnoreNotify");
    QNetworkReply *reply=manager->get(request);
    QEventLoop loop;
    connect(reply, SIGNAL(readyRead()), &loop, SLOT(quit()));
    loop.exec();
    std::cout<<"WbPoster"<<reply->readAll().data()<<std::endl;
    return -1;

}

void WebPoster::closeNotification(int id){
    //not supportted
}




#include "webposter.moc"
