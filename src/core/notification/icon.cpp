/****************************************************************************************
 * Copyright (c) 2011-2012 Patrick von Reth <patrick.vonreth@gmail.com>                 *
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

#include "icon.h"
#include "../snore.h"

#include <QCryptographicHash>
#include <QBuffer>
#include <QHash>
#include <QFile>
#include <QDebug>
namespace Snore{

class SnoreIcon::SnoreIconData : public QObject
{
    Q_OBJECT
public:
    SnoreIconData():
        m_isLocalFile(false)
    {	}

    SnoreIconData(const QImage &img):
        m_img(img),
        m_isLocalFile(false)
    {}

    SnoreIconData(const QString &url){
        qDebug()<<"Creating SnoreIcon"<<url;
        if(url.startsWith(":/"))
        {
            m_img = QImage(url);
            m_isLocalFile = false;
        }
        else if(QFile(url).exists())
        {
            m_isLocalFile = true;
            m_localUrl = url;
        }
        else
        {
            m_url = url;
            m_isLocalFile = false;
        }

    }

    ~SnoreIconData()
    {}


    QImage m_img;
    QByteArray m_data;
    QString m_localUrl;
    QString m_url;
    QString m_hash;
    bool m_isLocalFile;

};


QHash<QString,QString> SnoreIcon::hasedImages;

SnoreIcon::SnoreIcon()
{
    d = QSharedPointer<SnoreIconData>(new SnoreIconData());
}

SnoreIcon::SnoreIcon(const QImage &img)
{
    d = QSharedPointer<SnoreIconData>(new SnoreIconData(img));
}

SnoreIcon::SnoreIcon(const QString &url){
    d = QSharedPointer<SnoreIconData>(new SnoreIconData(url));
}

SnoreIcon::SnoreIcon(const SnoreIcon &other):
    d(other.d)
{    }

SnoreIcon::~SnoreIcon()
{
    d.clear();
}


const QImage &SnoreIcon::image() const{
    if(d->m_img.isNull() && d->m_isLocalFile){
        d->m_img = QImage(d->m_localUrl);
    }
    return d->m_img;
}

const QString &SnoreIcon::localUrl()const{
    if(d->m_localUrl.isEmpty()){
        if(hasedImages.contains(hash())){
            d->m_localUrl =  hasedImages[hash()];
        }else{
            d->m_localUrl = SnoreCore::snoreTMP();
            d->m_localUrl  = d->m_localUrl.append(hash()).append(".png");
            hasedImages[hash()] = d->m_localUrl;
            d->m_img.save(d->m_localUrl ,"PNG");
        }
    }
    return d->m_localUrl;
}

const QByteArray &SnoreIcon::imageData() const{
    if(d->m_data.isEmpty()){
        QBuffer buffer( &d->m_data );
        buffer.open( QBuffer::WriteOnly );
        d->m_img.save( &buffer, "PNG" );
    }
    return d->m_data;
}

const QString &SnoreIcon::hash() const{
    if(d->m_hash.isEmpty()){
        QCryptographicHash h(QCryptographicHash::Md5);
        h.addData(imageData());
        d->m_hash = h.result().toHex();
    }
    return d->m_hash;
}

bool SnoreIcon::isLocalFile() const
{
    return d->m_isLocalFile;
}

bool SnoreIcon::isEmpty() const{
    return d->m_hash.isEmpty() && d->m_img.isNull() && d->m_localUrl.isEmpty();
}

const QString &SnoreIcon::url() const
{
    d->m_url.isEmpty()?localUrl():d->m_url;
}

}

#include "icon.moc"
