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

#ifndef ICONDATA_H
#define ICONDATA_H


#include "notification.h"

#include <QImage>
#include <QSharedData>
#include <QBuffer>
#include <QHash>
#include <QFile>
#include <QDebug>
#include <QCryptographicHash>

namespace Snore{

class IconData : public QSharedData
{
public:
    IconData(const QString &url):
        m_url(url),
        m_isLocalFile(false)
    {
        if(m_url.startsWith(":/"))
        {
            m_hash = computeHash(m_url.toLatin1());
        }
        else if(QFile(url).exists())
        {
            m_isLocalFile = true;
            m_localUrl = url;
        }
    }

    IconData(const QImage &img):
        m_img(img),
        m_isLocalFile(false)
    {
        setImageData();
    }

    ~IconData()
    {

    }

    void setImageData()
    {
        QBuffer buffer( &m_data );
        buffer.open( QBuffer::WriteOnly );
        m_img.save( &buffer, "PNG" );

        if(m_hash.isEmpty())
        {
            m_hash = computeHash(m_data);
        }
    }

    static QString computeHash(const QByteArray &data)
    {
        QCryptographicHash h(QCryptographicHash::Md5);
        h.addData(data);
        return h.result().toHex();
    }

    QImage m_img;
    QByteArray m_data;
    QString m_localUrl;
    QString m_url;
    QString m_hash;
    bool m_isLocalFile;

private:
    Q_DISABLE_COPY(IconData)

};

}
#endif // ICONDATA_H
