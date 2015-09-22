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

#include <QDesktopWidget>
#include <QQmlProperty>

using namespace Snore;

NotifyWidget::NotifyWidget(int pos, const SnoreNotifier *parent) :
    m_id(pos),
    m_parent(parent),
    m_mem(QLatin1String("SnoreNotifyWidget_rev") + QString::number(SHARED_MEM_TYPE_REV()) + QLatin1String("_id") + QString::number(m_id)),
    m_ready(true)
{
    rootContext()->setContextProperty(QLatin1String("window"), this);

    QString font = qApp->font().family();
#ifdef Q_OS_WIN
    if (QSysInfo::windowsVersion() >= QSysInfo::WV_WINDOWS8) {
        font = QLatin1String("Segoe UI Symbol");
    }
#if QT_VERSION >= QT_VERSION_CHECK(5,5,0)
    if (QSysInfo::windowsVersion() >= QSysInfo::WV_WINDOWS10) {
        font = QLatin1String("Segoe UI Emoji");
    }
#endif
#endif
    rootContext()->setContextProperty(QLatin1String("snoreFont"), font);
    setSource(QUrl::fromEncoded("qrc:/notification.qml"));
    m_appIcon = rootObject()->findChild<QObject*>(QLatin1String("appIcon"));
    m_image = rootObject()->findChild<QObject*>(QLatin1String("image"));
    m_title = rootObject()->findChild<QObject*>(QLatin1String("title"));
    m_body = rootObject()->findChild<QObject*>(QLatin1String("body"));
    m_animation = rootObject()->findChild<QObject*>(QLatin1String("animation"));

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
        color = computeBackgrondColor(notification.application().icon().pixmap(QSize(20,20)).toImage());
        notification.application().hints().setPrivateValue(parent(), "backgroundColor", color);
    }
    QColor textColor = compueTextColor(color);
    int appIconWidht = m_appIcon->property("width").toInt();
    m_appIcon->setProperty("source", QUrl::fromLocalFile(notification.application().icon().localUrl(QSize(appIconWidht,appIconWidht))));
    int imageWidth = m_image->property("width").toInt();
    m_image->setProperty("source", QUrl::fromLocalFile(notification.icon().localUrl(QSize(imageWidth,imageWidth))));


    m_title->setProperty("text", notification.title(Utils::ALL_MARKUP));
    m_title->setProperty("color", textColor);

    m_body->setProperty("text", notification.text(Utils::ALL_MARKUP));
    m_body->setProperty("color", textColor);
    rootObject()->setProperty("color", color);


    if (!notification.isUpdate()) {
        QDesktopWidget desktop;
        double space = (id() + 1) * height() * 0.025;
        setY(space + (space + height()) * id());
        if (corner() == Qt::TopRightCorner || corner() == Qt::BottomRightCorner) {
            m_animation->setProperty("from", desktop.availableGeometry().width());
            m_animation->setProperty("to", desktop.availableGeometry().width() - width());
        } else {
            m_animation->setProperty("from", -width());
            m_animation->setProperty("to", 0);
        }
        if (corner() == Qt::TopRightCorner || corner() == Qt::TopLeftCorner) {
            setY(space + (space + height()) * id());
        } else {
            setY(desktop.availableGeometry().height() - (space + (space + height()) * (id() + 1)));
        }

        QMetaObject::invokeMethod(m_animation, "start");
        setVisible(true);
        Utils::raiseWindowToFront(winId());
    }
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
    int stepSize = img.depth()/8;
    qulonglong r = 0;
    qulonglong g = 0;
    qulonglong b = 0;
    const uchar *end = img.constBits() + img.byteCount();
    for (const uchar* bit = img.constBits(); bit != end; bit += stepSize ) {
        const QRgb c = *reinterpret_cast<const QRgb*>(bit);
        r += qRed(c);
        g += qGreen(c);
        b += qBlue(c);
    }
    int size = img.byteCount()/stepSize;
    return QColor(r / size, g / size, b / size);
}

QColor NotifyWidget::compueTextColor(const QColor &backgroundColor)
{
    // based on http://stackoverflow.com/a/946734
    QRgb compColor = qGray(backgroundColor.rgb()) > 186 ? 0 : 255;
    return QColor(compColor, compColor, compColor);
}
