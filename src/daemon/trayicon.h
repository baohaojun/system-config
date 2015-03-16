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

#include <QAction>
#include "core/snore.h"

namespace Snore
{
class SettingsDialog;
}

class TrayIcon: public QObject
{
    Q_OBJECT
public:
    TrayIcon();
    void initConextMenu();
    void hide();
    class QSystemTrayIcon *trayIcon();

private:
    class QSystemTrayIcon *m_trayIcon;
    class QMenu *m_trayMenu;
    QActionGroup *m_backendActions;
    Snore::SettingsDialog *m_settings;

    QHash<QTimer *, Snore::Notification> m_notifications;

public slots:
    void slotTestNotification();
    void sloutUpdateTestNotification();
    void slotSettings();
};

#endif // TRAYICON_H
