/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2015  Patrick von Reth <vonreth@kde.org>

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
#ifndef UTILS_H
#define UTILS_H

#include "snore_exports.h"

#include <QApplication>

namespace Snore {
class SNORE_EXPORT  Utils : public QObject {
    Q_OBJECT
public:
    Utils(QObject *parent = nullptr);
    ~Utils();

    //TODO: make Wid usable with the meta system and change signature.
    Q_INVOKABLE void bringWindowToFront(qlonglong wid);

};

}

#endif // UTILS_H
