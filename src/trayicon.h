/****************************************************************************************
 * Copyright (c) 2010 Patrick von Reth <patrick.vonreth@gmail.com>                      *
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

#ifndef TRAYICON_H
#define TRAYICON_H

#include <QtCore>

class TrayIcon:public QObject
{
    Q_OBJECT
public:
    TrayIcon(class QSystemTrayIcon *trayIcon,class SnoreServer *snore);
    void initConextMenu();

private:
    class QSystemTrayIcon *_trayIcon;
    class QMenu *_trayMenu;
    class SnoreServer *_snore;


public slots:
    void setPrimaryBackend();
};

#endif // TRAYICON_H
