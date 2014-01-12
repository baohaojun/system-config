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

#include "notification/icon_p.h"

using namespace Snore;


QHash<QString,QString> Icon::m_localImageCache;

Icon::Icon() :
    d(NULL)
{
}

Icon::Icon(const QImage &img):
    d(new IconData(img))
{
}

Icon::Icon(const QString &url):
    d(new IconData(url))
{
}

Icon::Icon(const Icon &other):
    d(other.d)
{
}

Icon &Icon::operator=(const Icon &other)
{
    d = other.d;
    return *this;
}

Icon::~Icon()
{

}

const QImage &Icon::image() const{
    if(d->m_img.isNull())
    {
        d->m_img = QImage(d->m_url);
    }
    return d->m_img;
}

QString Icon::localUrl()const{
    if(d->m_localUrl.isEmpty())
    {
        if(m_localImageCache.contains(hash()))
        {
            d->m_localUrl =  m_localImageCache[hash()];
        }
        else
        {
            d->m_localUrl = QString("%1%2.png").arg(SnoreCorePrivate::snoreTMP(), hash());
            image().save(d->m_localUrl ,"PNG");
            m_localImageCache[hash()] = d->m_localUrl;
        }
    }
    return d->m_localUrl;
}

const QByteArray &Icon::imageData() const{
    if(d->m_data.isEmpty() && !image().isNull()){
        d->setImageData();
    }
    return d->m_data;
}

QString Icon::hash() const{
    if(d->m_isLocalFile)
    {
        return "";
    }
    if(d->m_hash.isEmpty() && !imageData().isNull()){
        d->m_hash = IconData::computeHash(imageData());
    }
    return d->m_hash;
}

bool Icon::isLocalFile() const
{
    return d->m_isLocalFile;
}

bool Icon::isEmpty() const{
    return d->m_hash.isEmpty() && d->m_img.isNull() && d->m_localUrl.isEmpty();
}

bool Icon::isValid() const
{
    return d;
}

QString Icon::url() const
{
    return d->m_url;
}
