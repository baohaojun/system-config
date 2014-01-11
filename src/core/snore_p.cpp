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


#include "snore_p.h"
#include "snore.h"
#include "plugins/plugins.h"
#include "plugins/snorebackend.h"
#include "plugins/snorefrontend.h"

#include <QApplication>

using namespace Snore;


QString const SnoreCorePrivate::snoreTMP(){
    static const QString tmp = QString("%1/SnoreNotify/").arg(QDir::temp().path());
    return tmp;
}

const QDir &SnoreCorePrivate::pluginDir(){
    static QDir path(QString("%1/snoreplugins").arg(qApp->applicationDirPath()));
    if(!path.exists())
    {
        path = QDir(LIBSNORE_PLUGIN_PATH);
    }
    return path;
}


SnoreCorePrivate::SnoreCorePrivate(QSystemTrayIcon *trayIcon):
    m_trayIcon(trayIcon)
{
    QDir home ( snoreTMP() );
    if ( !home.exists() ){
        home.cdUp();
        home.mkdir("SnoreNotify");
    }
}

SnoreCorePrivate::~SnoreCorePrivate()
{

}

void SnoreCorePrivate::notificationActionInvoked(Notification notification) const
{
    Q_Q(const SnoreCore);
    emit const_cast<SnoreCore*>(q)->actionInvoked(notification);
    SnoreFrontend *nf = notification.source();
    if ( nf != NULL )
    {
        nf->actionInvoked ( notification );
    }
}

void SnoreCorePrivate::slotNotificationClosed(Notification n)
{
    Q_Q(SnoreCore);
    emit q->notificationClosed(n);
}

