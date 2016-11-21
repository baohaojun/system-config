/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2014-2015  Hannah von Reth <vonreth@kde.org>

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

#include "snorenotifier.h"
#include "notifywidget.h"
#include "libsnore/notification/notification_p.h"
#include "libsnore/snore_p.h"

#include <QThread>

SnorePlugin::Snore::Snore():
    m_timer(new QTimer(this))
{
    m_timer->setInterval(1000);
    connect(m_timer, &QTimer::timeout, this, &Snore::slotQueueTimeout);

}

SnorePlugin::Snore::~Snore()
{
    qDeleteAll(m_widgets);
}

void SnorePlugin::Snore::slotNotify(::Snore::Notification notification)
{
    auto display = [this](NotifyWidget * w, ::Snore::Notification notification) {
        w->display(notification);
        notification.hints().setPrivateValue(this, "id", w->id());
        slotNotificationDisplayed(notification);
    };

    if (notification.isUpdate()) {
        if (notification.old().hints().privateValue(this, "id").isValid()) {
            NotifyWidget *w = m_widgets[notification.old().hints().privateValue(this, "id").toInt()];
            if (w->notification().isValid() && w->notification().id() == notification.old().id()) {
                qCDebug(SNORE) << "replacing notification" << w->notification().id() << notification.id();
                display(w, notification);
            }
        } else {
            for (int i = 0; i < m_queue.length(); ++i) {
                ::Snore::Notification n = m_queue.at(i);
                if (n.id() == notification.old().id()) {
                    qCDebug(SNORE) << "replacing qued notification" << n.id() << notification.id();
                    m_queue.replace(i, notification);
                }
            }
        }
        return;
    }
    if (m_queue.isEmpty()) {
        foreach(NotifyWidget * w, m_widgets) {
            if (w->acquire(notification.timeout())) {
                display(w, notification);
                return;
            }
        }
    }
    m_queue.append(notification);
    qCDebug(SNORE) << "queueing" << m_queue.size();
    if (!m_timer->isActive()) {
        m_timer->start();
    }
}

void SnorePlugin::Snore::slotCloseNotification(::Snore::Notification notification)
{
    NotifyWidget *w = m_widgets[notification.hints().privateValue(this, "id").toInt()];
    w->release();
    slotQueueTimeout();
}

void SnorePlugin::Snore::slotRegisterApplication(const ::Snore::Application &)
{
    if (!m_widgets.isEmpty())
        return;
    m_widgets.resize(3);
    for (int i = 0; i < m_widgets.size(); ++i) {
        auto *w = new NotifyWidget(i, this);
        m_widgets[i] = w;
        connect(w, &NotifyWidget::dismissed, [this, w]() {
            ::Snore::Notification notification = w->notification();
            closeNotification(notification, ::Snore::Notification::Dismissed);
            slotCloseNotification(notification);
        });

        connect(w, &NotifyWidget::invoked, [this, w]() {
            ::Snore::Notification notification = w->notification();
            slotNotificationActionInvoked(notification);
            closeNotification(notification, ::Snore::Notification::Activated);
            slotCloseNotification(notification);
        });
    }


}

void SnorePlugin::Snore::slotQueueTimeout()
{
    if (m_queue.isEmpty()) {
        qCDebug(SNORE) << "queue is empty";
        m_timer->stop();
    } else {
        foreach(NotifyWidget * w, m_widgets) {
            if (!m_queue.isEmpty() && w->acquire(m_queue.first().timeout())) {
                ::Snore::Notification notification = m_queue.takeFirst();
                notification.hints().setPrivateValue(this, "id", w->id());
                w->display(notification);
                slotNotificationDisplayed(notification);
            }
        }
    }
}

bool SnorePlugin::Snore::canCloseNotification() const
{
    return true;
}

bool SnorePlugin::Snore::canUpdateNotification() const
{
    return true;
}

void SnorePlugin::Snore::setDefaultSettings()
{
    setDefaultSettingsValue(QStringLiteral("Position"), Qt::TopRightCorner);
    SnoreBackend::setDefaultSettings();
}
