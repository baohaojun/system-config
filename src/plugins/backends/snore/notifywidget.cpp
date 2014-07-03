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

#include "notifywidget.h"
#include "core/log.h"

#include <QDesktopWidget>
#include <QDesktopServices>
#include <QPicture>
#include <QtDeclarative/QDeclarativeView>
#include <QLayout>
#include <QSize>

using namespace Snore;

NotifyWidget::NotifyWidget(int pos,QWidget *parent) :
    QDeclarativeView(QUrl("qrc:/notification.qml"), parent),
    m_moveTimer(new QTimer(this)),
    m_desktop(QDesktopWidget().availableGeometry()),
    m_id(pos),
    m_mem(QString("SnoreNotifyWidget%1").arg(QString::number(m_id))),
    m_ready(true)
{
    qmlNotification = rootObject();
    this->setWindowFlags(Qt::ToolTip | Qt::WindowStaysOnTopHint | Qt::FramelessWindowHint
                     #if QT_VERSION >= QT_VERSION_CHECK(5,0,0)
                         | Qt::WindowDoesNotAcceptFocus
                     #endif
                         );
    if(m_mem.create(sizeof(SHARED_MEM_TYPE)))
    {
        m_mem.lock();
        bool *data = (bool*)m_mem.data();
        *data = true;
        m_mem.unlock();
    }
    else
    {
        if(!m_mem.attach())
        {
            qFatal("Failed to atatche to shared mem");
        }
        m_mem.lock();
        bool *data = (bool*)m_mem.data();
        m_mem.unlock();
        snoreDebug( SNORE_DEBUG ) << "Status" << *data;
    }




    m_dest = QPoint(m_desktop.topRight().x() - width(), m_desktop.topRight().y() + 10 + 10 + height() * pos);
    m_start = QPoint(m_desktop.topRight().x(), m_dest.y());

    m_moveTimer->setInterval(1);
    connect( m_moveTimer, SIGNAL(timeout()), this, SLOT(slotMove()));

    connect(qmlNotification, SIGNAL(invoked()),this, SLOT(slotInvoked()));
    connect(qmlNotification, SIGNAL(dismissed()),this, SLOT(slotDismissed()));
    connect(qmlNotification, SIGNAL(linkClicked(QString)),this, SLOT(slotLinkClicked(QString)));
}

NotifyWidget::~NotifyWidget()
{
}

void NotifyWidget::display(const Notification &notification)
{
    update(notification);
    m_dist = 0;
    m_moveTimer->start();
    snoreDebug( SNORE_DEBUG ) << notification.id();
    move(m_start);
    show();
}

void NotifyWidget::update(const Notification &notification)
{
    m_notification = notification;
    QColor color = computeBackgrondColor(notification.application().icon().image());
    QRgb gray = qGray(qGray(color.rgb()) - qGray(QColor(Qt::white).rgb()));
    QColor textColor = QColor(gray, gray, gray);
    QMetaObject::invokeMethod(qmlNotification, "update", Qt::QueuedConnection,
                              Q_ARG( QVariant, notification.title()),
                              Q_ARG( QVariant, notification.text()),
                              Q_ARG( QVariant, QUrl::fromLocalFile(notification.icon().localUrl())),
                              Q_ARG( QVariant, QUrl::fromLocalFile(notification.application().icon().localUrl())),
                              Q_ARG( QVariant, color),
                              Q_ARG( QVariant, textColor ));


}

bool NotifyWidget::acquire()
{
    bool out = false;
    if(m_ready)
    {
        snoreDebug( SNORE_DEBUG ) << m_id;
        m_mem.lock();
        bool *data = (bool*)m_mem.data();
        if(*data)
        {
            *data = false;
            m_ready = false;
            out = true;
        }
        m_mem.unlock();
    }
    return out;
}

bool NotifyWidget::release()
{
    bool out = false;
    if(!m_ready)
    {
        snoreDebug( SNORE_DEBUG ) << m_id;
        m_mem.lock();
        bool *data = (bool*)m_mem.data();
        if(!*data)
        {
            *data = true;
            m_ready = true;
            out = true;
        }
        m_mem.unlock();
        hide();
    }
    return out;
}

Notification &NotifyWidget::notification()
{
    return m_notification;
}

int NotifyWidget::id()
{
    return m_id;
}

void NotifyWidget::slotMove()
{
    QPoint dest(m_start.x() - m_dist++, m_start.y());
    move(dest);
    if(m_dist >= width())
    {
        m_moveTimer->stop();
    }
}

void NotifyWidget::slotDismissed()
{
    emit dismissed();
}

void NotifyWidget::slotInvoked()
{
    emit invoked();
}

QColor NotifyWidget::computeBackgrondColor(const QImage &img)
{
    qulonglong r = 0;
    qulonglong g = 0;
    qulonglong b = 0;
    for(int x=0;x<img.width();++x)
    {
        for(int y=0;y<img.height();++y)
        {
            QRgb c = img.pixel(x,y);
            r += qRed(c);
            g += qGreen(c);
            b += qBlue(c);
        }
    }
    int s = img.width()*img.height();

    return QColor(r/s, g/s, b/s);

}

void NotifyWidget::slotLinkClicked(QString link)
{
    QDesktopServices::openUrl( QUrl(link));
}
