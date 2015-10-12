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

#include "notifywidget.h"
#include "snorenotifier.h"
#include "libsnore/log.h"
#include "libsnore/utils.h"

#include <QApplication>
#include <QDesktopWidget>
#include <QQmlProperty>

using namespace Snore;

NotifyWidget::NotifyWidget(int id, const SnoreNotifier *parent) :
    m_id(id),
    m_parent(parent),
    m_mem(QLatin1String("SnoreNotifyWidget_rev") + QString::number(SHARED_MEM_TYPE_REV()) + QLatin1String("_id") + QString::number(m_id)),
    m_ready(true),
    m_fontFamily(qApp->font().family())
{

#ifdef Q_OS_WIN
    if (QSysInfo::windowsVersion() >= QSysInfo::WV_WINDOWS8) {
        m_fontFamily = QLatin1String("Segoe UI Symbol");
        emit fontFamilyChanged();
    }
#if QT_VERSION >= QT_VERSION_CHECK(5,5,0)
    if (QSysInfo::windowsVersion() >= QSysInfo::WV_WINDOWS10) {
        m_fontFamily = QLatin1String("Segoe UI Emoji");
        emit fontFamilyChanged();
    }
#endif
#endif
    QQmlApplicationEngine *engine = new QQmlApplicationEngine(this);
    engine->rootContext()->setContextProperty(QLatin1String("notifyWidget"), this);
    engine->load(QUrl::fromEncoded("qrc:/notification.qml"));
    m_window = qobject_cast<QQuickWindow *>(engine->rootObjects().value(0));

    // TODO: It looks like there is a Qt bug wich make some Windows with this flag invisible in some cases...(Tested: Kubuntu willy)
    m_window->setFlags(Qt::WindowStaysOnTopHint | Qt::ToolTip
#ifdef Q_OS_MAC
                       // TODO: is this needed or is ToolTip working?
                       //| Qt::SubWindow
#endif
                      );

    if (m_mem.create(sizeof(SHARED_MEM_TYPE))) {
        m_mem.lock();
        SHARED_MEM_TYPE *data = (SHARED_MEM_TYPE *)m_mem.data();
        data->free = true;
        data->date = QTime::currentTime().msecsSinceStartOfDay();
        m_mem.unlock();
    } else {
        if (!m_mem.attach()) {
            snoreDebug(SNORE_WARNING) << "Failed to atatche to shared mem";
        } else {
            m_mem.lock();
            SHARED_MEM_TYPE *data = (SHARED_MEM_TYPE *)m_mem.data();
            m_mem.unlock();
            int elapsed = (QTime::currentTime().msecsSinceStartOfDay() - data->date) / 1000;
            snoreDebug(SNORE_DEBUG) << m_id << "State:" << data->free << "Time:" << elapsed << "Timeout:" << data->timeout;
        }
    }
}

NotifyWidget::~NotifyWidget()
{
    release();
}

void NotifyWidget::display(const Notification &notification)
{
    snoreDebug(SNORE_DEBUG) << m_id << notification.id() << m_window->isVisible();
    m_notification = notification;
    QColor color;
    QVariant vcolor = notification.application().constHints().privateValue(parent(), "backgroundColor");
    if (vcolor.isValid()) {
        color = vcolor.value<QColor>();
    } else {
        color = computeBackgrondColor(notification.application().icon().pixmap(QSize(20, 20)).toImage());
        notification.application().hints().setPrivateValue(parent(), "backgroundColor", color);
    }
    m_appIcon = QUrl::fromLocalFile(notification.application().icon().localUrl(QSize(m_appIconSize, m_appIconSize)));
    emit appIconChanged();

    m_image = QUrl::fromLocalFile(notification.icon().localUrl(QSize(m_imageSize, m_imageSize)));
    emit imageChanged();

    m_title = notification.title(Utils::ALL_MARKUP);
    emit titleChanged();
    m_body = notification.text(Utils::ALL_MARKUP);
    emit bodyChanged();

    if (!notification.isUpdate()) {
        syncSettings();
        m_color = color;
        m_textColor = compueTextColor(color);
        emit colorChanged();
        emit textColorChanged();
        m_window->show();
        Utils::raiseWindowToFront(m_window->winId());
    }
}

bool NotifyWidget::acquire(int timeout)
{
    if (!m_mem.isAttached()) {
        return true;
    }
    bool out = false;
    if (m_ready) {
        m_mem.lock();
        SHARED_MEM_TYPE *data = (SHARED_MEM_TYPE *)m_mem.data();
        int elapsed = (QTime::currentTime().msecsSinceStartOfDay() - data->date) / 1000;
        snoreDebug(SNORE_DEBUG) << m_id << "State:" << data->free << "Time:" << elapsed << "Timeout:" << data->timeout;
        bool isTimedOut = elapsed > data->timeout;
        if (data->free || isTimedOut) {
            if (isTimedOut) {
                snoreDebug(SNORE_DEBUG) << "Notification Lock timed out" << elapsed;
            }
            data->free = false;
            data->date = QTime::currentTime().msecsSinceStartOfDay();
            data->timeout = timeout;
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
        int elapsed = (QTime::currentTime().msecsSinceStartOfDay() - data->date) / 1000;
        snoreDebug(SNORE_DEBUG) << m_id << "State:" << data->free << "Time:" << elapsed << "Timeout:" << data->timeout;
        if (!data->free) {
            data->free = true;
            m_ready = true;
            out = true;
        }
        m_mem.unlock();
        m_window->hide();
    }
    return out;
}

Notification &NotifyWidget::notification()
{
    return m_notification;
}

int NotifyWidget::id() const
{
    return m_id;
}

void NotifyWidget::syncSettings()
{
    Qt::Corner c = static_cast<Qt::Corner>(m_parent->settingsValue(QLatin1String("Position")).toInt());
    if (c != m_cornerOld || !m_initialized) {
        m_initialized = true;
        QDesktopWidget desktop;

        m_cornerOld = c;
        m_isOrientatedLeft = c == Qt::TopLeftCorner || c == Qt::BottomLeftCorner;
        if (m_isOrientatedLeft) {
            m_dragMinX = m_animationFrom = -m_window->width();
            m_dragMaxX = m_animationTo = 0;
        } else {
            m_animationFrom = desktop.availableGeometry().width();
            m_animationTo = desktop.availableGeometry().width() - m_window->width();
            m_dragMinX =  0;
            m_dragMaxX = m_window->width();

        }
        double space = (id() + 1) * m_window->height() * 0.025;

        if (c == Qt::TopRightCorner || c == Qt::TopLeftCorner) {
            m_window->setY(space + (space + m_window->height()) * id());
        } else {
            m_window->setY(desktop.availableGeometry().height() - (space + (space + m_window->height()) * (id() + 1)));
        }
        emit isOrientatedLeftChanged();
        emit animationFromChanged();
        emit animationtoChanged();
        emit dragMaxXChanged();
        emit dragMinXChanged();
    }
}

QColor NotifyWidget::computeBackgrondColor(const QImage &img)
{
    int stepSize = img.depth() / 8;
    qulonglong r = 0;
    qulonglong g = 0;
    qulonglong b = 0;
    const uchar *end = img.constBits() + img.byteCount();
    for (const uchar *bit = img.constBits(); bit != end; bit += stepSize) {
        const QRgb c = *reinterpret_cast<const QRgb *>(bit);
        r += qRed(c);
        g += qGreen(c);
        b += qBlue(c);
    }
    int size = img.byteCount() / stepSize;
    return QColor(r / size, g / size, b / size);
}

QColor NotifyWidget::compueTextColor(const QColor &backgroundColor)
{
    // based on http://stackoverflow.com/a/946734
    QRgb compColor = qGray(backgroundColor.rgb()) > 186 ? 0 : 255;
    return QColor(compColor, compColor, compColor);
}

