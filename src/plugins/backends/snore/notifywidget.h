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

#ifndef NOTIFYWIDGET_H
#define NOTIFYWIDGET_H

#include <QSharedMemory>
#include "libsnore/notification/notification.h"

#include <QtQuick/QtQuick>

class SnoreNotifier;

typedef struct {
    bool free;
    int date;
    int timeout;

} SHARED_MEM_TYPE;

inline int SHARED_MEM_TYPE_REV()
{
    return 2;
}

class NotifyWidget : public QObject
{
    Q_OBJECT
    Q_PROPERTY(int id READ id)
    Q_PROPERTY(Qt::Corner position MEMBER m_corner NOTIFY positionChanged)
    Q_PROPERTY(QColor color MEMBER m_color NOTIFY colorChanged)
    Q_PROPERTY(QColor textColor MEMBER m_textColor NOTIFY textColorChanged)
    Q_PROPERTY(QString title MEMBER m_title NOTIFY titleChanged)
    Q_PROPERTY(QString body MEMBER m_body NOTIFY bodyChanged)
    Q_PROPERTY(int imageSize MEMBER m_imageSize)
    Q_PROPERTY(int appIconSize MEMBER m_appIconSize)
    Q_PROPERTY(QUrl image MEMBER m_image NOTIFY imageChanged)
    Q_PROPERTY(QUrl appIcon MEMBER m_appIcon NOTIFY appIconChanged)
    Q_PROPERTY(QString fontFamily MEMBER m_fontFamily NOTIFY fontFamilyChanged)

public:
    explicit NotifyWidget(int id, const SnoreNotifier *parent);
    ~NotifyWidget();

    void display(const Snore::Notification &notification);

    bool acquire(int timeout);
    bool release();

    Snore::Notification &notification();

    int id() const;

Q_SIGNALS:
    void invoked();
    void dismissed();

    void positionChanged();

    void textColorChanged();
    void colorChanged();
    void titleChanged();
    void bodyChanged();

    void imageChanged();
    void appIconChanged();

    void fontFamilyChanged();

private:
    void syncSettings();
    QColor computeBackgrondColor(const QImage &img);
    QColor compueTextColor(const QColor &backgroundColor);

    void updateCornerValues(Qt::Corner c);

    Snore::Notification m_notification;
    int m_id;
    const SnoreNotifier *m_parent;
    QSharedMemory m_mem;
    bool m_ready;

    Qt::Corner m_corner = Qt::TopLeftCorner;

    int m_imageSize;
    int m_appIconSize;

    QColor m_color;
    QColor m_textColor;

    QString m_title;
    QString m_body;

    QString m_fontFamily;

    bool m_initialized = false;

    QUrl m_appIcon;
    QUrl m_image;

    QQuickWindow *m_window;

};

#endif // NOTIFYWIDGET_H
