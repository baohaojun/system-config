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

#ifndef SNORENOTIFY_H
#define SNORENOTIFY_H

#include <QObject>

class TrayIcon;

namespace Snore
{
class SnoreCore;
}

class SnoreNotify: public QObject
{
    Q_OBJECT
public:
    SnoreNotify();
    ~SnoreNotify();

private:
    TrayIcon *m_trayIcon;
};

#endif // SNORENOTIFY_H
