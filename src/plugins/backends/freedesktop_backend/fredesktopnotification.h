/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2015  Hannah von Reth <vonreth@kde.org>

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

#ifndef FreedesktopNotification_H
#define FreedesktopNotification_H

#include "libsnore/notification/notification.h"

#include <QMetaType>
#include <QDBusArgument>

class FreedesktopImageHint
{
public:
    FreedesktopImageHint() = default;
    FreedesktopImageHint(const QImage &img);
    QImage toQImage() const;

    int width;
    int height;
    int rowstride;
    bool hasAlpha;
    int bitsPerSample;
    int channels;
    QByteArray imageData;
    QString _hash;
private:
    static int imageHintID;

};

Q_DECLARE_METATYPE(FreedesktopImageHint)

QDBusArgument &operator<< (QDBusArgument &a,  const FreedesktopImageHint &i);
const QDBusArgument &operator >> (const QDBusArgument &a,  FreedesktopImageHint  &i) ;

#endif // FreedesktopNotification_H
