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

#include <QEventLoop>
#include <QNetworkAccessManager>
#include <QNetworkReply>

using namespace Snore;

IconData::IconData(const QString &url):
    m_url(url),
    m_hash(computeHash(m_url.toLatin1())),
    m_isLocalFile(false),
    m_isResource(m_url.startsWith(":/"))
{
    if(!m_isResource && QFile(url).exists())
    {
        m_isLocalFile = true;
        m_localUrl = url;
    }
    else
    {
        m_localUrl = QString("%1%2.png").arg(SnoreCorePrivate::snoreTMP(), m_hash);
    }
    m_isRemoteFile = !m_isLocalFile && ! m_isResource;
}

IconData::IconData(const QImage &img):
    m_img(img),
    m_isLocalFile(false),
    m_isResource(false),
    m_isRemoteFile(false)
{
    imageData();
}

IconData::~IconData()
{

}


const QByteArray &Snore::IconData::imageData()
{
    if(m_data.isEmpty())
    {
        if(!m_isLocalFile)
        {
            if(!m_img.isNull())
            {
                QBuffer buffer( &m_data );
                buffer.open( QBuffer::WriteOnly );
                m_img.save( &buffer, "PNG" );
                if(m_hash.isEmpty())
                {
                    m_hash = computeHash(m_data);
                }
            }
            else if(m_isRemoteFile)
            {
                download();
            }
        }
    }
    return m_data;
}

QString IconData::computeHash(const QByteArray &data)
{
    QCryptographicHash h(QCryptographicHash::Md5);
    h.addData(data);
    return h.result().toHex();
}

void IconData::download()
{
    QNetworkAccessManager manager;
    QEventLoop loop;
    QNetworkRequest request(m_url);
    request.setRawHeader("User-Agent", "SnoreNotify");
    QNetworkReply *reply = manager.get(request);
    QObject::connect(reply, SIGNAL(finished()), &loop, SLOT(quit()));
    loop.exec();
    m_data = reply->readAll();
}
