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


QHash<QString,QString> SnoreIcon::hasedImages;

SnoreIcon::SnoreIcon() :
    d(NULL)
{
}

SnoreIcon::SnoreIcon(const QImage &img):
    d(new SnoreIconData(img))
{

}

SnoreIcon::SnoreIcon(const QString &url):
    d(new SnoreIconData(url))
{

}

SnoreIcon::SnoreIcon(const SnoreIcon &other):
    d(other.d)
{
    qDebug() << *this;
}

SnoreIcon::~SnoreIcon()
{

}


const QImage &SnoreIcon::image() const{
    if(d->m_img.isNull()){
        d->m_img = QImage(d->m_url);
    }
    return d->m_img;
}

QString SnoreIcon::localUrl()const{
    if(d->m_localUrl.isEmpty())
    {
        if(hasedImages.contains(hash()))
        {
            d->m_localUrl =  hasedImages[hash()];
        }
        else
        {
            d->m_localUrl = QString("%1%2.png").arg(SnoreCore::snoreTMP(), hash(), ".png");
            image().save(d->m_localUrl ,"PNG");
            hasedImages[hash()] = d->m_localUrl;
        }
    }
    return d->m_localUrl;
}

const QByteArray &SnoreIcon::imageData() const{
    if(d->m_data.isEmpty() && !image().isNull()){
        d->setImageData();
    }
    return d->m_data;
}

QString SnoreIcon::hash() const{
    if(d->m_isLocalFile)
    {
        return "";
    }
    if(d->m_hash.isEmpty() && !imageData().isNull()){
        d->m_hash = computeHash(imageData());
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

bool SnoreIcon::isValid() const
{
    return d;
}

QString SnoreIcon::computeHash(const QByteArray &data)
{
    QCryptographicHash h(QCryptographicHash::Md5);
    h.addData(data);
    return h.result().toHex();
}

QString SnoreIcon::url() const
{
    return d->m_url;
}

SnoreIcon::SnoreIcon::SnoreIconData::SnoreIconData(const QString &url):
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

SnoreIcon::SnoreIcon::SnoreIconData::SnoreIconData(const QImage &img):
    m_img(img),
    m_isLocalFile(false)
{
    setImageData();
}

SnoreIcon::SnoreIcon::SnoreIconData::~SnoreIconData()
{

}

void SnoreIcon::SnoreIconData::setImageData()
{
    QBuffer buffer( &m_data );
    buffer.open( QBuffer::WriteOnly );
    m_img.save( &buffer, "PNG" );

    if(m_hash.isEmpty())
    {
        m_hash = computeHash(m_data);
    }
}

}
