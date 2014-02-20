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
#include "DpiScaler.h"

namespace Ui {
class NotifyWidget;
}

typedef bool SHARED_MEM_TYPE;
class NotifyWidget : public QWidget
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

    void on_closeButton_clicked();

protected:
    void mousePressEvent(QMouseEvent *e);



private:

    void setPalette(const QImage &img);
    Ui::NotifyWidget *ui;
    QTimer *m_moveTimer;
    QPoint m_dest;
    QPoint m_start;
    QRect m_desktop;
    TomahawkUtils::DpiScaler *m_scaler;
    Snore::Notification m_notification;

    int m_id;
    QSharedMemory m_mem;
};

#endif // NOTIFYWIDGET_H
