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

#ifndef NOTIFICATION_ICON_H
#define NOTIFICATION_ICON_H
#include "../snore_exports.h"

#include <QSharedData>
#include <QDebug>

namespace Snore{

class IconData;

class SNORE_EXPORT Icon
{
public:
    Icon();
    Icon(const QImage &img);
    Icon(const class QString &url);
    Icon(const Icon &other);
    Icon &operator=(const Icon &other);
    ~Icon();

    const QImage &image() const;
    QString localUrl() const;
    QString url() const;
    const QByteArray &imageData() const ;
    QString hash() const;
    bool isLocalFile() const;
    bool isEmpty() const;
    bool isValid() const;


private:
    static QHash<QString,QString> m_localImageCache;

    QExplicitlySharedDataPointer<IconData> d;
};
}


inline QDebug operator<< ( QDebug debug, const Snore::Icon &icon )
{
    if(icon.isValid())
    {
        debug << "Snore::Icon(" << icon.url() << ")" ;
    }
    else
    {
        debug << "Snore::Icon(0x00)" ;
    }
    return debug.maybeSpace();
}


#endif // NOTIFICATION_ICON_H
