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

#include "icon_p.h"
#include "../snore_p.h"
#include "../utils.h"

#include <QApplication>
#include <QFile>
#include <QNetworkAccessManager>
#include <QNetworkReply>

using namespace Snore;

QSet<QString> IconData::s_localImageCache;

IconData::IconData(const QString &url):
    m_url(url),
    m_hash(Utils::computeMD5Hash(m_url.toLatin1())),
    m_localUrl(createLocalFileName(m_hash)),
    m_isLocalFile(false),
    m_isResource(m_url.startsWith(":/") || m_url.startsWith("qrc:/"))
{
    if (!m_isResource && QFile::exists(url)) {
        m_isLocalFile = true;
        m_localUrl = url;
    }
    m_isRemoteFile = !m_isLocalFile && !m_isResource;

    if (!m_isLocalFile && !s_localImageCache.contains(m_localUrl)) {
        if (m_isRemoteFile) {
            m_isDownloading = true;
            snoreDebug(SNORE_DEBUG) << "Downloading:" << m_url;
            QNetworkAccessManager *manager = new QNetworkAccessManager();
            QNetworkRequest request(m_url);
            QNetworkReply *reply = manager->get(request);
//            QObject::connect(reply, &QNetworkReply::downloadProgress, [&](qint64 bytesReceived, qint64 bytesTotal) {
//                snoreDebug(SNORE_DEBUG) << "Downloading:" << m_localUrl << bytesReceived / double(bytesTotal) * 100.0 << "%";
//            });

            QObject::connect(reply, static_cast<void (QNetworkReply::*)(QNetworkReply::NetworkError)>(&QNetworkReply::error), [ &, reply, manager](QNetworkReply::NetworkError code) {
                snoreDebug(SNORE_WARNING) << "Error:" << code;
                reply->deleteLater();
                manager->deleteLater();
                m_isDownloading = false;
            });
            QObject::connect(reply, &QNetworkReply::finished, [ &, reply, manager]() {
                m_img = QImage::fromData(reply->readAll(), "PNG");
                m_img.save(m_localUrl, "PNG");
                s_localImageCache.insert(m_localUrl);
                snoreDebug(SNORE_DEBUG) << m_localUrl << "added to cache";
                reply->close();
                reply->deleteLater();
                manager->deleteLater();
                m_isDownloading = false;
            });
        } else if (m_isResource) {
            m_img = QImage(url);
            m_img.save(m_localUrl, "PNG");
            s_localImageCache.insert(m_localUrl);
            snoreDebug(SNORE_DEBUG) << m_localUrl << "added to cache";
        }
    }

}

IconData::IconData(const QImage &img):
    m_img(img),
    m_hash(Utils::computeMD5Hash(Icon::dataFromImage(img))),
    m_localUrl(createLocalFileName(m_hash)),
    m_isLocalFile(false),
    m_isResource(false),
    m_isRemoteFile(false)
{
    if (!s_localImageCache.contains(m_localUrl)) { //double check as image() could have called download
        img.save(m_localUrl , "PNG");
        s_localImageCache.insert(m_localUrl);
        snoreDebug(SNORE_DEBUG) << m_localUrl << "added to cache";
    }
}

IconData::~IconData()
{
}

const QImage &IconData::image()
{
    if (m_img.isNull()) {
        while (m_isDownloading) {
            qApp->processEvents();
        }
        if (!m_isRemoteFile) {
            m_img = QImage(m_url);
        }
    }
    return m_img;
}

QString IconData::localUrl()
{
    while (m_isDownloading) {
        qApp->processEvents();
    }
    return m_localUrl;
}
