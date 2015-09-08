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
#include "snorenotifier.h"
#include "libsnore/log.h"
#include "libsnore/utils.h"

using namespace Snore;

NotifyWidget::NotifyWidget(int pos, const SnoreNotifier *parent) :
    m_id(pos),
    m_parent(parent),
    m_mem(QLatin1String("SnoreNotifyWidget_rev") + QString::number(SHARED_MEM_TYPE_REV()) + QLatin1String("_id") + QString::number(m_id)),
    m_ready(true)
{
    rootContext()->setContextProperty(QLatin1String("window"), this);
    rootContext()->setContextProperty(QLatin1String("utils"), new Utils(this));

    QString font = qApp->font().family();
#ifdef Q_OS_WIN
    if(QSysInfo::windowsVersion() >= QSysInfo::WV_WINDOWS8 ) {
        font = QLatin1String("Segoe UI Symbol");
    }
#if QT_VERSION >= QT_VERSION_CHECK(5,5,0)
    if(QSysInfo::windowsVersion() >= QSysInfo::WV_WINDOWS10) {
        font = QLatin1String("Segoe UI Emoji");
    }
#endif
#endif
    rootContext()->setContextProperty(QLatin1String("snoreFont"), font);
    setSource(QUrl::fromEncoded("qrc:/notification.qml"));

    setFlags(Qt::WindowStaysOnTopHint | Qt::FramelessWindowHint | Qt::WindowSystemMenuHint | Qt::WindowDoesNotAcceptFocus
#ifdef Q_OS_MAC
             | Qt::SubWindow
#else
             | Qt::Tool
#endif
            );

//    setFocusPolicy(Qt::NoFocus);
//    setAttribute(Qt::WA_ShowWithoutActivating, true);

    if (m_mem.create(sizeof(SHARED_MEM_TYPE))) {
        m_mem.lock();
        SHARED_MEM_TYPE *data = (SHARED_MEM_TYPE *)m_mem.data();
        data->free = true;
        data->date = QTime::currentTime();
        m_mem.unlock();
    } else {
        if (!m_mem.attach()) {
            snoreDebug(SNORE_WARNING) << "Failed to atatche to shared mem";
        } else {
            m_mem.lock();
            SHARED_MEM_TYPE *data = (SHARED_MEM_TYPE *)m_mem.data();
            m_mem.unlock();
            snoreDebug(SNORE_DEBUG) << "Status" << data->free << data->date.elapsed() / 1000;
        }
    }

    setResizeMode(QQuickView::SizeViewToRootObject);

    connect(rootObject(), SIGNAL(invoked()), this, SLOT(slotInvoked()));
    connect(rootObject(), SIGNAL(dismissed()), this, SLOT(slotDismissed()));
}

NotifyWidget::~NotifyWidget()
{
    release();
}

void NotifyWidget::display(const Notification &notification)
{
    snoreDebug(SNORE_DEBUG) << m_id << notification.id();
    m_notification = notification;
    QColor color;
    QVariant vcolor = notification.application().constHints().privateValue(parent(), "backgroundColor");
    if (vcolor.isValid()) {
        color = vcolor.value<QColor>();
    } else {
        color = computeBackgrondColor(notification.application().icon().image().scaled(20, 20));
        notification.application().hints().setPrivateValue(parent(), "backgroundColor", color);
    }
    QRgb gray = qGray(qGray(color.rgb()) - qGray(QColor(Qt::white).rgb()));
    QColor textColor = QColor(gray, gray, gray);
    QMetaObject::invokeMethod(rootObject(), "update", Qt::QueuedConnection,
                              Q_ARG(QVariant, notification.title(Utils::ALL_MARKUP)),
                              Q_ARG(QVariant, notification.text(Utils::ALL_MARKUP)),
                              Q_ARG(QVariant, QUrl::fromLocalFile(notification.icon().localUrl())),
                              Q_ARG(QVariant, QUrl::fromLocalFile(notification.application().icon().localUrl())),
                              Q_ARG(QVariant, color),
                              Q_ARG(QVariant, textColor),
                              Q_ARG(QVariant, notification.isUpdate()));
}

bool NotifyWidget::acquire()
{
    if (!m_mem.isAttached()) {
       return true;
    }
    bool out = false;
    if (m_ready) {
        m_mem.lock();
        SHARED_MEM_TYPE *data = (SHARED_MEM_TYPE *)m_mem.data();
        snoreDebug(SNORE_DEBUG) << m_id << data->free << data->date.elapsed() / 1000;
        bool timedout = data->date.elapsed() / 1000 > 60;
        if (data->free || timedout) {
            if (timedout) {
                snoreDebug(SNORE_DEBUG) << "Notification Lock timed out" << data->date.elapsed() / 1000;
            }
            data->free = false;
            data->date = QTime::currentTime();
            m_ready = false;
            out = true;
        }
        m_mem.unlock();
    }
    return out;
}

bool NotifyWidget::release()
{
    if (!m_mem.isAttached()) {
       return true;
    }
    bool out = false;
    if (!m_ready) {
        m_mem.lock();
        SHARED_MEM_TYPE *data = (SHARED_MEM_TYPE *)m_mem.data();
        snoreDebug(SNORE_DEBUG) << m_id << data->free << data->date.elapsed() / 1000 << m_notification.id();
        if (!data->free) {
            data->free = true;
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

Qt::Corner NotifyWidget::corner()
{
    return static_cast<Qt::Corner>(m_parent->settingsValue(QLatin1String("Position")).toInt());
}

qlonglong NotifyWidget::wid()
{
    return this->winId();
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
    for (int x = 0; x < img.width(); ++x) {
        for (int y = 0; y < img.height(); ++y) {
            QRgb c = img.pixel(x, y);
            r += qRed(c);
            g += qGreen(c);
            b += qBlue(c);
        }
    }
    int s = img.width() * img.height();

    return QColor(r / s, g / s, b / s);

}
