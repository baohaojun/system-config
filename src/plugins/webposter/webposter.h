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

#ifndef WEBPOSTER_H
#define WEBPOSTER_H
#include "core/snore_exports.h"
#include "core/interface.h"
#include <QtNetwork>


class WebPoster: public Notification_Backend{
    Q_OBJECT
    Q_INTERFACES(Notification_Backend)
public:
    WebPoster(class SnoreServer *snore=0);
    bool isPrimaryNotificationBackend(){return false;}

public slots:
        void registerApplication(Application *application);
        int notify(QSharedPointer<Notification>notification);
        void closeNotification(int id);

private:
        QNetworkAccessManager *manager;


};
#endif//WEBPOSTER_H
