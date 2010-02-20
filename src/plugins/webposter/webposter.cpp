#include "webposter.h"
#include <QDebug>
#include <QtCore>
Q_EXPORT_PLUGIN2(webposter,WebPoster)

WebPoster::WebPoster():manager(new QNetworkAccessManager(this)){
    setProperty("name","WebPoster");
}

int WebPoster::notify(QSharedPointer<Notification>notification){
    QByteArray byte(notification->toSnalrString().toLatin1().data());
    QUrl url("http://www.pro-zeit.ch/index.php");
    url.addEncodedQueryItem("action","add");
    url.addEncodedQueryItem("data",QUrl::toPercentEncoding(byte.toBase64()));
    QNetworkRequest request(url);
    request.setRawHeader("User-Agent", "SnoreNotify");
    QNetworkReply *reply=manager->get(request);
    connect(reply, SIGNAL(readyRead()), this, SLOT(slotReadyRead()));
    return -1;


}

void WebPoster::closeNotification(int id){
//not supportted
}

void WebPoster::slotReadyRead(){
    QNetworkReply *reply=qobject_cast<QNetworkReply*>(sender());
    qDebug()<<reply->url();
    qDebug()<<reply->readAll();
    if(reply->errorString()!="Unknown error")
        qWarning()<<reply->errorString();
    reply->deleteLater();
}


#include "webposter.moc"
