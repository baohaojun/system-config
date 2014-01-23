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

#ifndef NOTIFICATION_ICON_H
#define NOTIFICATION_ICON_H
#include "../snore_exports.h"

#include <QSharedData>
#include <QDebug>
namespace Snore{
    class Icon;
}

SNORE_EXPORT QDebug operator<< ( QDebug, const Snore::Icon &);

namespace Snore{
class IconData;

/**
 * Icon contains an image for Notifications.
 * @author Patrick von Reth \<vonreth at kde.org\>
 */
class SNORE_EXPORT Icon
{
public:
    Icon();
    Icon(const QImage &img);
    explicit Icon(const QString &url);

    Icon(const Icon &other);
    Icon &operator=(const Icon &other);
    ~Icon();

    const QImage &image() const;
    QString localUrl() const;
    QString url() const;
    const QByteArray &imageData() const ;
    bool isLocalFile() const;
    bool isRemoteFile() const;
    bool isValid() const;

private:
    QExplicitlySharedDataPointer<IconData> d;
    friend SNORE_EXPORT QDebug (::operator<<) ( QDebug, const Snore::Icon &);
};
}



#endif // NOTIFICATION_ICON_H
