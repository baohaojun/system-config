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
#include "ui_notifywidget.h"
#include "core/log.h"

#include <QDesktopWidget>
#include <QPicture>

using namespace Snore;

NotifyWidget::NotifyWidget(int pos,QWidget *parent) :
    QWidget(parent, Qt::Tool | Qt::WindowStaysOnTopHint | Qt::FramelessWindowHint
            #if QT_VERSION >= QT_VERSION_CHECK(5,0,0)
            | Qt::WindowDoesNotAcceptFocus
            #endif
            ),
    ui(new Ui::NotifyWidget),
    m_moveTimer(new QTimer(this)),
    m_desktop(QDesktopWidget().availableGeometry()),
    m_id(pos),
    m_mem(QString("SnoreNotifyWidget%1").arg(QString::number(m_id))),
    m_ready(true)
{
    ui->setupUi(this);
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

    TomahawkUtils::DpiScaler::setFontSize(10);
    m_scaler = new TomahawkUtils::DpiScaler(this);
    ui->closeButton->setMaximumWidth(ui->closeButton->height());

    setFixedSize( m_scaler->scaled(300, 80));

    m_dest = QPoint(m_desktop.topRight().x() - width(), m_desktop.topRight().y() + m_scaler->scaledY(10) + (m_scaler->scaledY(10) + height()) * pos);
    m_start = QPoint(m_desktop.topRight().x(), m_dest.y());
    snoreDebug( SNORE_DEBUG ) << m_dest << m_start << size();

    m_moveTimer->setInterval(2);
    connect( m_moveTimer, SIGNAL(timeout()), this, SLOT(slotMove()));

#if QT_VERSION >= QT_VERSION_CHECK(5,0,0)//ugly bug in qt4 makes the widget resize
    ui->titel->setWordWrap(true);
    ui->body->setWordWrap(true);
#endif

}

NotifyWidget::~NotifyWidget()
{
    delete m_scaler;
    delete ui;
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
    ui->titel->setText(notification.title());
    ui->body->setText(notification.text());

    QSize iconSize = m_scaler->scaled(65,65);
    ui->icon->setPixmap(QPixmap::fromImage(notification.icon().image().scaled(iconSize, Qt::KeepAspectRatio, Qt::SmoothTransformation)));

    iconSize = m_scaler->scaled(20,20);
    QImage img = notification.application().icon().image().scaled(iconSize, Qt::KeepAspectRatio, Qt::SmoothTransformation);
    ui->appIcon->setPixmap(QPixmap::fromImage(img));
    setPalette(img);
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

void NotifyWidget::on_closeButton_clicked()
{
    emit dismissed();
}

void NotifyWidget::mousePressEvent(QMouseEvent *)
{
    emit invoked();
}

void NotifyWidget::setPalette(const QImage &img)
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

    QPalette p = palette();
    QColor bg = QColor(r/s, g/s, b/s);
    p.setColor(QPalette::All, QPalette::Window, bg);
    p.setColor(QPalette::All, QPalette::Background, bg);
    p.setColor(QPalette::All, QPalette::Base, bg);
    p.setColor(QPalette::All, QPalette::Text, Qt::white);
    p.setColor(QPalette::All, QPalette::BrightText, Qt::white);
    p.setColor(QPalette::All, QPalette::ButtonText, Qt::white);
    p.setColor(QPalette::All, QPalette::WindowText, Qt::white);
    QWidget::setPalette(p);
    ui->closeButton->setPalette(p);
    ui->body->setPalette(p);
    ui->titel->setPalette(p);
}

void NotifyWidget::on_body_linkActivated(const QString &link)
{
    snoreDebug( SNORE_DEBUG ) << link;
}
