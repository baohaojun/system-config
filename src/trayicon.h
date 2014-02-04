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

#ifndef TRAYICON_H
#define TRAYICON_H

#include <QtCore>
#include <QAction>
#include "core/snore.h"


class TrayIcon:public QObject
{
    Q_OBJECT
public:
    TrayIcon();
    void initConextMenu(Snore::SnoreCore *snore);
    void hide();
    class QSystemTrayIcon* trayIcon();

private:
    class QSystemTrayIcon *m_trayIcon;
    class QMenu *m_trayMenu;
    QActionGroup *m_backendActions;
    Snore::SnoreCore *m_snore;
    Snore::Application m_app;
    Snore::Alert m_alert;
    Snore::Notification m_noti;


public slots:
    void setPrimaryBackend();
    void slotTestNotification();
    void sloutUpdateTestNotification();
};

#endif // TRAYICON_H
