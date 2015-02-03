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

#include "fredesktopnotification.h"

#include <QImage>
#include <QDBusMetaType>

int FreedesktopImageHint::imageHintID = qDBusRegisterMetaType<FreedesktopImageHint>();

FreedesktopImageHint::FreedesktopImageHint()
{

}

FreedesktopImageHint::FreedesktopImageHint(const QImage &img)
{
    QImage image(img.convertToFormat(QImage::Format_ARGB32));
    imageData = QByteArray((char *)image.rgbSwapped().bits(), image.byteCount());
    width = image.width();
    height = image.height();
    rowstride = image.bytesPerLine();
    hasAlpha = image.hasAlphaChannel();
    channels = hasAlpha ? 4 : 3;
    bitsPerSample = image.depth() / channels;

}

QImage FreedesktopImageHint::toQImage() const
{
    return QImage((uchar *)imageData.data(), width, height, QImage::Format_ARGB32).rgbSwapped();
}

QDBusArgument &operator<<(QDBusArgument &a, const FreedesktopImageHint &i)
{
    a.beginStructure();
    a << i.width <<
      i.height <<
      i.rowstride <<
      i.hasAlpha <<
      i.bitsPerSample <<
      i.channels <<
      i.imageData;
    a.endStructure();
    return a;
}

const QDBusArgument &operator >>(const QDBusArgument &a,  FreedesktopImageHint &i)
{
    a.beginStructure();
    a >> i.width >>
      i.height >>
      i.rowstride >>
      i.hasAlpha >>
      i.bitsPerSample >>
      i.channels >>
      i.imageData;
    a.endStructure();
    return a;
}
