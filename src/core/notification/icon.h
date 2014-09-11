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
#include "snore_exports.h"

#include <QSharedData>
#include <QDebug>

namespace Snore
{
class Icon;
}

SNORE_EXPORT QDebug operator<< (QDebug, const Snore::Icon &);

namespace Snore
{
class IconData;

/**
 * Icon contains an image for Notifications.
 * Icon uses a shared datamodel, it's content is never copied and automatically released.
 * @author Patrick von Reth \<vonreth at kde.org\>
 */
class SNORE_EXPORT Icon
{
public:

    static QByteArray dataFromImage(const QImage &image);

    Icon();

    /**
     * Creates an Icon from an QImage
     * @param img the image
     */
    Icon(const QImage &img);

    /**
     * Creates an Icon from a url
     * Valid urls are "file://home/foo/foo.png", "C:\\foo.png", ":/root/foo.png", "http://foo.com/foo.png"
     * @param url the url
     */
    explicit Icon(const QString &url);

    /**
     * Creates a copy of other
     * @param other
     */
    Icon(const Icon &other);

    /**
     * Creates a copy of other
     * @param other
     */
    Icon &operator=(const Icon &other);
    ~Icon();

    /**
     *
     * @return a QImage from the Icon
     */
    const QImage &image() const;

    /**
     *
     * @return a local url to a file representing the Icon
     */
    QString localUrl() const;

    /**
     *
     * @return the url of this Icon or an empty string if created from a QImage
     */
    QString url() const;

    /**
     *
     * @return whether the Icon was created from a local file
     */
    bool isLocalFile() const;

    /**
     *
     * @return whether the Icon was created from a remote file
     */
    bool isRemoteFile() const;

    /**
     *
     * @return whether this is a valid Icon
     */
    bool isValid() const;

    Icon scaled(const QSize &s) const;

private:
    QExplicitlySharedDataPointer<IconData> d;
    friend SNORE_EXPORT QDebug(::operator<<)(QDebug, const Snore::Icon &);
};
}

#endif // NOTIFICATION_ICON_H
