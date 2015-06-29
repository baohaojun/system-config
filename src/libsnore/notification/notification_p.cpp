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

#include "notification/notification_p.h"
#include "notification/icon.h"
#include "../hint.h"
#include "libsnore/log.h"
#include "libsnore/plugins/plugins.h"
#include "libsnore/snore.h"

#include <QSharedData>

using namespace Snore;

uint NotificationData::notificationCount = 0;

uint NotificationData::m_idCount = 1;

NotificationData::NotificationData(const Snore::Application &application, const Snore::Alert &alert, const QString &title, const QString &text, const Icon &icon,
                                   int timeout, Notification::Prioritys priority):
    m_id(m_idCount++),
    m_timeout(priority == Notification::EMERGENCY ? 0 : timeout),
    m_application(application),
    m_alert(alert),
    m_title(title),
    m_text(text),
    m_icon(icon),
    m_priority(priority),
    m_closeReason(Notification::NONE)
{
    notificationCount++;
    snoreDebug(SNORE_INFO) << "Creating Notification: ActiveNotifications" << notificationCount << "id" << m_id;
    initHints();
}

Snore::NotificationData::NotificationData(const Notification &old, const QString &title, const QString &text, const Icon &icon, int timeout, Notification::Prioritys priority):
    m_id(m_idCount++),
    m_timeout(priority == Notification::EMERGENCY ? 0 : timeout),
    m_application(old.application()),
    m_alert(old.alert()),
    m_title(title),
    m_text(text),
    m_icon(icon),
    m_priority(priority),
    m_closeReason(Notification::NONE),
    m_toReplace(old)
{
    notificationCount++;
    initHints();
    snoreDebug(SNORE_INFO) << "Creating Notification: ActiveNotifications" << notificationCount << "id" << m_id;
}

NotificationData::~NotificationData()
{
    notificationCount--;
    snoreDebug(SNORE_INFO) << "Deleting Notification: ActiveNotifications" << notificationCount << "id" << m_id << "Close Reason:" << m_closeReason;
}

void NotificationData::setActionInvoked(const Snore::Action &action)
{
    m_actionInvoked = action;
}

void NotificationData::setCloseReason(Snore::Notification::CloseReasons r)
{
    m_closeReason = r;
}

void NotificationData::setTimeoutTimer(QTimer *timer)
{
    if (m_timeoutTimer) {
        m_timeoutTimer->stop();
        m_timeoutTimer->deleteLater();
    }
    m_timeoutTimer.reset(timer);
}

QString NotificationData::resolveMarkup(const QString &string, Utils::MARKUP_FLAGS flags)
{
    if(flags != Utils::NO_MARKUP && !m_application.constHints().value("use-markup").toBool()) {
        return string.toHtmlEscaped();
    } else {
        return Utils::normalizeMarkup(string, flags);
    }
}

void NotificationData::initHints()
{
    m_hints.setValue("silent", SnoreCore::instance().value("Silent", LOCAL_SETTING));
}

