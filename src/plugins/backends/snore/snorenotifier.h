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

#ifndef SNORENOTIFIER_H
#define SNORENOTIFIER_H

#include "core/plugins/snorebackend.h"

#include "notifywidget.h"

class SnoreNotifier : public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationBackend/1.0" FILE "plugin.json")
public:
    SnoreNotifier();
    ~SnoreNotifier();

    virtual bool initialize(Snore::SnoreCore *snore);
    virtual bool deinitialize();

public slots:
    virtual void slotNotify(Snore::Notification notification);
    virtual void slotCloseNotification(Snore::Notification notification);

private slots:
    void slotDismissed();
    void slotInvoked();
    void slotProcessQueue();

    void setup();

private:

    QList<Snore::Notification> m_queue;
    QVector<NotifyWidget *> m_widgets;
    QTimer *m_timer;

};

#endif // SNORENOTIFIER_H
