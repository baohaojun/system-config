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

#include <QBuffer>

using namespace Snore;

QByteArray Icon::dataFromImage(const QImage &image)
{
    QByteArray data;
    QBuffer buffer(&data);
    buffer.open(QBuffer::WriteOnly);
    image.save(&buffer, "PNG");
    return data;
}

Icon Icon::defaultIcon()
{
    static Icon icon(QLatin1String(":/root/snore.png"));
    return icon;
}

Icon::Icon(const QImage &img):
    d(new IconData(img))
{
}

Icon::Icon(const QIcon &icon):
    d(new IconData(icon.pixmap(32).toImage()))
{}

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

const QImage &Icon::image() const
{
    return d->image();
}

QString Icon::localUrl()const
{
    return d->localUrl();
}

bool Icon::isLocalFile() const
{
    return d->m_isLocalFile;
}

bool Icon::isValid() const
{
    return !(d->m_img.isNull() && d->m_url.isEmpty());
}

Icon Icon::scaled(const QSize &s) const
{
    return Icon(image().scaled(s, Qt::KeepAspectRatio, Qt::SmoothTransformation));
}

QString Icon::url() const
{
    return d->m_url;
}

bool Snore::Icon::isRemoteFile() const
{
    return d->m_isRemoteFile;
}

QDebug operator<< (QDebug debug, const Snore::Icon &icon)
{
    debug << "Snore::Icon(" << (icon.url().isEmpty() ? icon.d->m_localUrl : icon.url()) <<  ")" ;
    return debug.maybeSpace();
}
