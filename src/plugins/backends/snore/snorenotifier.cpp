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


#include "snorenotifier.h"
#include "notifywidget.h"
#include "core/notification/notification_p.h"
#include "core/snore_p.h"
#include <QApplication>

Q_EXPORT_PLUGIN2(libsnore_backend_snore,SnoreNotifier)

using namespace Snore;

SnoreNotifier::SnoreNotifier():
    SnoreBackend("Snore", true, true, true),
    m_widgets(2)
{
}

SnoreNotifier::~SnoreNotifier()
{
    foreach (NotifyWidget *w, m_widgets)
    {
        w->deleteLater();
    }
}

void SnoreNotifier::slotNotify(Snore::Notification notification)
{

    if(notification.isUpdate())
    {
        NotifyWidget *w = m_widgets[notification.old().hints().privateValue(this, "id").toInt()];
        w->update(notification);
        notification.hints().setPrivateValue(this, "id", w->id());
        startTimeout(notification);
        return;
    }
    else if(m_queue.isEmpty())
    {
        foreach (NotifyWidget *w, m_widgets)
        {
            if(w->isHidden())
            {
                w->display(notification);
                notification.hints().setPrivateValue(this, "id", w->id());
                startTimeout(notification);
                return;
            }
        }
    }
    m_queue.append(notification);
}

void SnoreNotifier::slotCloseNotification(Snore::Notification notification)
{

    NotifyWidget *w = m_widgets[notification.hints().privateValue(this, "id").toInt()];
    if(!m_queue.isEmpty())
    {
        Notification nextNoti = m_queue.takeLast();
        w->display(nextNoti);
        nextNoti.hints().setPrivateValue(this, "id", w->id());
        startTimeout(nextNoti);
    }
    else
    {
        w->hide();
    }
}


void SnoreNotifier::slotDismissed()
{
    NotifyWidget *widget = qobject_cast<NotifyWidget*>(sender());
    Notification notification = widget->notification();
    closeNotification(notification, Notification::DISMISSED);
    slotCloseNotification(notification);
}

void SnoreNotifier::slotInvoked()
{
    NotifyWidget *widget = qobject_cast<NotifyWidget*>(sender());
    Notification notification = widget->notification();
    snore()->d()->notificationActionInvoked(notification);
    closeNotification(notification,Notification::CLOSED);
    slotCloseNotification(notification);
}

void SnoreNotifier::setup()
{
    for(int i=0;i<m_widgets.size();++i)
    {
        NotifyWidget *w = new NotifyWidget(i);
        m_widgets[i] = w;
        connect(w, SIGNAL(dismissed()), this, SLOT(slotDismissed()));
        connect(w, SIGNAL(invoked()), this, SLOT(slotInvoked()));
    }
}


bool SnoreNotifier::initialize(SnoreCore *snore)
{
    if(SnoreBackend::initialize(snore))
    {
        return metaObject()->invokeMethod(this, "setup", Qt::QueuedConnection);
    }
    return false;
}

bool SnoreNotifier::deinitialize()
{
    if(SnoreBackend::deinitialize())
    {
        for(int i=0;i<m_widgets.size();++i)
        {
            m_widgets[i]->deleteLater();
        }
        return true;
    }
    return false;
}
