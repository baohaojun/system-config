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
#include "libsnore/snore_exports.h"

#include <QIcon>
#include <QSharedData>
#include <QDebug>


namespace Snore
{

/**
 * Icon contains an image for Notifications.
 * Icon uses a shared datamodel, it's content is never copied and automatically released.
 * @author Patrick von Reth \<vonreth at kde.org\>
 */
class SNORE_EXPORT Icon : public QIcon
{
public:
    static Icon defaultIcon();
    static Icon fromWebUrl(const QUrl& url, int maxTime = 5000);


    Icon(const QPixmap &pixmap);
    Icon(const QIcon &other);
    explicit Icon(const QString &fileName);


    /**
     *
     * @return a local url to a file representing the Icon
     */
    QString localUrl(const QSize &size, Mode mode = Normal, State state = Off) const;



private:
    Icon() = delete;
    static QSet<QString> s_localImageCache;
    static QMap<QUrl,Icon> s_downloadImageCache;
};
}

#endif // NOTIFICATION_ICON_H
