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

#ifndef NOTIFYWIDGET_H
#define NOTIFYWIDGET_H

#include <QWidget>
#include <QTimer>
#include <QSharedMemory>
#include "core/notification/notification.h"

#include <QtDeclarative>


typedef struct
{
    bool free;
    QTime date;

}SHARED_MEM_TYPE;

inline int SHARED_MEM_TYPE_REV()
{
    return 1;
}

class NotifyWidget : public QDeclarativeView
{
    Q_OBJECT

public:
    explicit NotifyWidget(int pos, QWidget *parent = 0);
    ~NotifyWidget();

    void display(const Snore::Notification &notification);
    void update(const Snore::Notification &notification);

    bool acquire();
    bool release();

    Snore::Notification &notification();

    int id();



signals:
    void invoked();
    void dismissed();

private slots:
    void slotMove();

    void slotDismissed();

    void slotInvoked();

protected:
    QSize computeSize();

    inline float dpisScale()
    {
        return 96.0;
    }

private:

    QColor computeBackgrondColor(const QImage &img);
    QTimer *m_moveTimer;
    QPoint m_dest;
    QPoint m_start;
    int m_dist;
    Snore::Notification m_notification;

    QObject *qmlNotification;

    int m_id;
    QSharedMemory m_mem;
    bool m_ready;
};

#endif // NOTIFYWIDGET_H
