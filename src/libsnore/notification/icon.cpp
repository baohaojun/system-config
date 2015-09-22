/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2014  Patrick von Reth <vonreth@kde.org>

    SnoreNotify is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    SnoreNotify is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with SnoreNotify.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "icon.h"
#include "../snore.h"
#include "../snore_p.h"

#include <QApplication>
#include <QMutex>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QUrl>



using namespace Snore;


QSet<QString> Icon::s_localImageCache;
QMap<QUrl,Icon> Icon::s_downloadImageCache;

Icon Icon::defaultIcon()
{
    static Icon icon(QLatin1String(":/root/snore.png"));
    return icon;
}

Icon Icon::fromWebUrl(const QUrl &url, int maxTime)
{
    Icon icon = defaultIcon();
    snoreDebug(SNORE_DEBUG) << url;
    if (!s_downloadImageCache.contains(url)) {
        QTime timeout;
        timeout.start();
        QMutex isDownloading;
        isDownloading.lock();
        snoreDebug(SNORE_DEBUG) << "Downloading:" << url;
        QNetworkAccessManager *manager = new QNetworkAccessManager();
        QNetworkRequest request(url);
        QNetworkReply *reply = manager->get(request);
        QObject::connect(reply, &QNetworkReply::downloadProgress, [&](qint64 bytesReceived, qint64 bytesTotal) {
            snoreDebug(SNORE_DEBUG) << "Downloading:" << url << bytesReceived / double(bytesTotal) * 100.0 << "%";
        });

        QObject::connect(reply, static_cast<void (QNetworkReply::*)(QNetworkReply::NetworkError)>(&QNetworkReply::error), [ & ](QNetworkReply::NetworkError code) {
            snoreDebug(SNORE_WARNING) << "Error downloading" << url << ":" << code;
            isDownloading.unlock();
        });
        QObject::connect(reply, &QNetworkReply::finished, [ & ]() {
            if(reply->isOpen()){
                QImage img(QImage::fromData(reply->readAll(), "PNG"));
                icon = Icon(QPixmap::fromImage(img));
                s_downloadImageCache.insert(url, icon);
                snoreDebug(SNORE_DEBUG) << url << "added to cache.";
                isDownloading.unlock();
            } else {
                snoreDebug(SNORE_DEBUG) << "Download of " << url << "timed out.";

            }
        });

        while(!isDownloading.tryLock() && timeout.elapsed() < maxTime)
        {
            qApp->processEvents();
        }
        reply->close();
        reply->deleteLater();
        manager->deleteLater();
    } else {
        icon = s_downloadImageCache.value(url, defaultIcon());
        snoreDebug(SNORE_DEBUG) << url << "from cache";
    }
    return icon;
}

Icon::Icon(const QPixmap &pixmap):
    QIcon(pixmap)
{

}

Icon::Icon(const QIcon &other):
    QIcon(other)
{

}

Icon::Icon(const QString &fileName):
    QIcon(fileName)
{

}

QString Icon::localUrl(const QSize &size, Mode mode, State state)const
{
    QString localFileName = SnoreCorePrivate::tempPath() + QLatin1Char('/') + QString::number(cacheKey()) + QLatin1String("_") + QString::number(size.width())+ QLatin1String("x") + QString::number(size.height()) + QLatin1String(".png");
    if(!s_localImageCache.contains(localFileName)){
        QImage(pixmap(size,mode,state).toImage()).save(localFileName, "PNG");
        s_localImageCache.insert(localFileName);
    }
    return localFileName;
}

