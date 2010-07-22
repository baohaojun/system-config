/****************************************************************************************
 * Copyright (c) 2010 Patrick von Reth <patrick.vonreth@gmail.com>                      *
 *                                                                                      *
 * This program is free software; you can redistribute it and/or modify it under        *
 * the terms of the GNU General Public License as published by the Free Software        *
 * Foundation; either version 2 of the License, or (at your option) any later           *
 * version.                                                                             *
 *                                                                                      *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY      *
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A      *
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.             *
 *                                                                                      *
 * You should have received a copy of the GNU General Public License along with         *
 * this program.  If not, see <http://www.gnu.org/licenses/>.                           *
 ****************************************************************************************/

#include "webposter.h"
#include <QDebug>
#include <QtCore>
#include <iostream>

#include "core/utils.h"

Q_EXPORT_PLUGIN2(webposter,WebPoster)

WebPoster::WebPoster(SnoreServer *snore):
Notification_Backend("WebPoster",snore)
{
    manager=new QNetworkAccessManager(this);
}

void WebPoster::registerApplication(Application *application){

}

void WebPoster::unregisterApplication(Application *application){

}

int WebPoster::notify(QSharedPointer<Notification>notification){
    QByteArray byte(Utils::notificationToSNTPString(notification).toLatin1().data());
    QUrl url("http://www.pro-zeit.ch/index.php");
    url.addEncodedQueryItem("action","add");
    url.addEncodedQueryItem("data",QUrl::toPercentEncoding(byte.toBase64()));
    QNetworkRequest request(url);
    request.setRawHeader("User-Agent", "SnoreNotify");
    QNetworkReply *reply=manager->get(request);
    QEventLoop loop;
    connect(reply, SIGNAL(readyRead()), &loop, SLOT(quit()));
    loop.exec();
    return -1;

}

void WebPoster::closeNotification(QSharedPointer<Notification> notification){
    //not supportted
}




#include "webposter.moc"
