/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2014  Patrick von Reth <vonreth@kde.org>


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


#ifndef SNORECOREPRIVATE_H
#define SNORECOREPRIVATE_H

#include "snore.h"
#include "plugins/snorebackend.h"

#include <QDir>
#include <QPointer>
#include <QCryptographicHash>

namespace Snore
{
class SNORE_EXPORT SnoreCorePrivate : public QObject
{
    Q_DECLARE_PUBLIC(SnoreCore)
    Q_OBJECT

public:
    static const QString snoreTMP();
    static const QDir &pluginDir();
    static inline QString computeHash(const QByteArray &data)
    {
        return QCryptographicHash::hash(data,QCryptographicHash::Md5).toHex();
    }
public:
    SnoreCorePrivate(QSystemTrayIcon *trayIcon);
    ~SnoreCorePrivate();
    const Application defaultApplication() const;


    void notificationActionInvoked(Notification notification) const;

    bool setBackendIfAvailible(const QString & backend);

signals:
    void applicationRegistered(const Snore::Application&);
    void applicationDeregistered(const Snore::Application&);
    void notify(Snore::Notification noti);

private slots:
    void slotNotificationClosed(Snore::Notification);

private:
    SnoreCore *q_ptr;

    QHash<QString,Application> m_applications;

    QMultiHash<SnorePlugin::PluginTypes, QString> m_plugins;

    QPointer<SnoreBackend> m_notificationBackend;

    QSystemTrayIcon *m_trayIcon;

    Application m_defaultApp;
};
}

#endif // SNORECOREPRIVATE_H
