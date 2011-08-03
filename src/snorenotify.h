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

#ifndef SNORENOTIFY_H
#define SNORENOTIFY_H

#include <QtCore>

class SnoreNotify:public QObject
{
    Q_OBJECT
public:
    SnoreNotify();
	~SnoreNotify();
	void load();
	void save();

private:
	class TrayIcon *_trayIcon;
	class SnoreServer *_snore;
        class QSettings _settings;

private slots:
    void exit();
};

#endif // SNORENOTIFY_H
