#include <QPainterPath>

#include "surfacewidget.h"
#include "qvncviewersettings.h"
#include "qt2keysum.h"
#include "macros.h"

extern QtVncViewerSettings *globalConfig;

SurfaceWidget::SurfaceWidget(ConnectionWindow *connectionWindow, QWidget *parent) :
    QFrame(parent),
    m_connectionWindow(connectionWindow),
    m_scale(1.0),
    m_sx(1.0),
    m_sy(1.0),
    m_surfacePixmap(0),
    m_bilinearFilter(true),
    m_scaled(true),
    m_keepAspect(true),
    m_showFps(false),
    m_buttonMask(0),
    m_frameCounter(0),
    m_currentFps(0)
{
    setFrameShadow(QFrame::Raised);
    setFrameShape(QFrame::StyledPanel);
    setFocusPolicy(Qt::StrongFocus);
    setMouseTracking(true);
    setBackgroundBrush(Qt::black);
    setForegroundBrush(Qt::white);
    setTextBackgroundBrush(QBrush(QColor(0, 0, 0, 128), Qt::SolidPattern));
    setDefaultMessage(tr("Disconnected"));
    setScaled(globalConfig->preferencesScaled());
    setBilinearFilter(globalConfig->preferencesBilinearFilter());
    setKeepAspect(globalConfig->preferencesKeepAspect());
    m_font = font();
    m_font.setPointSize(m_font.pointSize() * 2);
    m_font.setBold(true);
    connect(&m_frameTimer, SIGNAL(timeout()), this, SLOT(frameTimerTimeout()));
    m_frameTimer.setSingleShot(true);
    m_frameTimer.start(QVNCVIEWER_ONE_SECOND);
    installEventFilter(this);
}

SurfaceWidget::~SurfaceWidget()
{
    if ( surfacePixmap() )
        delete surfacePixmap();
}

void SurfaceWidget::setSurfaceSize(QSize surfaceSize)
{
    QPixmap *oldSP = surfacePixmap();
    m_surfacePixmap = new QPixmap(surfaceSize);
    surfacePixmap()->fill(backgroundBrush().color());
    m_surfaceRect = surfacePixmap()->rect();
    m_surfaceRect.setWidth(m_surfaceRect.width() * m_scale);
    m_surfaceRect.setHeight(m_surfaceRect.height() * m_scale);
    m_transform = QTransform::fromScale(1.0 / m_scale, 1.0 / m_scale);
    QTimer::singleShot(0, this, SLOT(updateSurface()));
    if ( oldSP )
        delete oldSP;
}

int SurfaceWidget::translateMouseButton(Qt::MouseButton button)
{
    switch ( button ) {
    case Qt::LeftButton:
        return rfbButton1Mask;
    case Qt::RightButton:
        return rfbButton2Mask;
    case Qt::MiddleButton:
        return rfbButton3Mask;
    default:
        return 0;
    }
}

rfbClient *SurfaceWidget::client()
{
    return connectionWindow()->rfbClientPtr();
}

ConnectionWindow *SurfaceWidget::connectionWindow()
{
    return m_connectionWindow;
}

void SurfaceWidget::updateSurface()
{
    m_font = font();
    m_font.setPointSize(m_font.pointSize() * 2);
    m_font.setBold(true);
    resizeEvent(0);
    update();
}

void SurfaceWidget::clearSurface()
{
    if ( surfacePixmap() )
        setSurfaceSize(surfacePixmap()->size());
    setCurrentFps(0);
    setFrameCounter(0);
}

void SurfaceWidget::frameTimerTimeout()
{
    setCurrentFps(frameCounter());
    setFrameCounter(0);
    m_frameTimer.start(QVNCVIEWER_ONE_SECOND);
}

bool SurfaceWidget::eventFilter(QObject *object, QEvent *event)
{
    switch ( event->type() ) {
    case QEvent::MouseMove: {
            QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
            if ( mouseEvent->pos().y() < QVNCVIEWER_MENUBAR_MOUSE_Y ) {
                setFrameShadow(QFrame::Raised);
                setFrameShape(QFrame::StyledPanel);
                connectionWindow()->setMenuFrameVisible(true);
                connectionWindow()->menuFrameTimer().start(QVNCVIEWER_MENUBAR_TIMEOUT);
            }
            if ( client() ) {
                QPoint mappedPos = m_transform.map(mouseEvent->pos());
                SendPointerEvent(client(), mappedPos.x(), mappedPos.y(), m_buttonMask);
            }
        }
        break;
    case QEvent::MouseButtonPress: {
            QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
            m_surfaceRect.moveCenter(rect().center());
            if ( connectionWindow()->menuFrameVisible() && connectionWindow()->isFullScreen() )
                connectionWindow()->setMenuFrameVisible(false);
            if ( client() ) {
                QPoint mappedPos = m_transform.map(mouseEvent->pos());
                m_buttonMask |= translateMouseButton(mouseEvent->button());
                SendPointerEvent(client(), mappedPos.x(), mappedPos.y(), m_buttonMask);
                m_buttonMask &= ~(rfbButton4Mask | rfbButton5Mask);
            }
        }
        break;
    case QEvent::MouseButtonRelease: {
            QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
            m_surfaceRect.moveCenter(rect().center());
            if ( client() ) {
                QPoint mappedPos = m_transform.map(mouseEvent->pos());
                m_buttonMask &= ~translateMouseButton(mouseEvent->button());
                SendPointerEvent(client(), mappedPos.x(), mappedPos.y(), m_buttonMask);
                m_buttonMask &= ~(rfbButton4Mask | rfbButton5Mask);
            }
        }
        break;
    case QEvent::MouseButtonDblClick: {
            QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
            m_surfaceRect.moveCenter(rect().center());
            if ( client() ) {
                QPoint mappedPos = m_transform.map(mouseEvent->pos());
                m_buttonMask |= translateMouseButton(mouseEvent->button());
                SendPointerEvent(client(), mappedPos.x(), mappedPos.y(), m_buttonMask);
                m_buttonMask &= ~translateMouseButton(mouseEvent->button());
                SendPointerEvent(client(), mappedPos.x(), mappedPos.y(), m_buttonMask);
                m_buttonMask |= translateMouseButton(mouseEvent->button());
                SendPointerEvent(client(), mappedPos.x(), mappedPos.y(), m_buttonMask);
                m_buttonMask &= ~translateMouseButton(mouseEvent->button());
                SendPointerEvent(client(), mappedPos.x(), mappedPos.y(), m_buttonMask);
                m_buttonMask &= ~(rfbButton4Mask | rfbButton5Mask);
            }
        }
        break;
    case QEvent::KeyPress: {
            QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
            if ( keyEvent->key() == Qt::Key_Alt ) {
                if ( client() ) {
                    SendKeyEvent(client(), qt2keysym(keyEvent->key()), true);
                    setFocus();
                    return true;
                }
            }
            if ( QVNCVIEWER_FULLSCREEN_TOGGLED )
                QTimer::singleShot(0, connectionWindow(), SLOT(on_toolButtonToggleFullScreen_clicked()));
            if ( client() ) {
                SendKeyEvent(client(), qt2keysym(keyEvent->key()), true);
                return true;
            }
        }
        break;
    case QEvent::KeyRelease: {
            QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
            if ( client() ) {
                if ( keyEvent->key() == Qt::Key_Alt )
                        setFocus();
                SendKeyEvent(client(), qt2keysym(keyEvent->key()), false);
                return true;
            }
        }
        break;
    default:
        break;
    }
    return QFrame::eventFilter(object, event);
}

void SurfaceWidget::resizeEvent(QResizeEvent *e)
{
    if ( surfacePixmap() ) {
        if ( scaled() ) {
            if ( !surfacePixmap()->isNull() ) {
                if ( surfacePixmap()->width() > surfacePixmap()->height() ) {
                    m_scale = (qreal)width() / (qreal)surfacePixmap()->width();
                    if ( surfacePixmap()->height() * m_scale > height() )
                        m_scale = (qreal)height()  / (qreal)surfacePixmap()->height();
                } else {
                    m_scale = (qreal)height() / (qreal)surfacePixmap()->height();
                    if ( surfacePixmap()->width() * m_scale > width() )
                        m_scale = (qreal)width() / (qreal)surfacePixmap()->width();
                }
            } else
                m_scale = 1.0;
        } else
            m_scale = 1.0;
        m_surfaceRect = surfacePixmap()->rect();
        if ( keepAspect() ) {
            m_surfaceRect.setWidth(m_surfaceRect.width() * m_scale);
            m_surfaceRect.setHeight(m_surfaceRect.height() * m_scale);
            m_surfaceRect.moveCenter(rect().center());
            if ( scaled() ) {
                m_transform = QTransform::fromScale(1.0 / m_scale, 1.0 / m_scale);
                m_transform.translate(-m_surfaceRect.x(), -m_surfaceRect.y());
            } else {
                m_transform = QTransform::fromScale(1, 1);
                m_transform.translate(-m_surfaceRect.x(), -m_surfaceRect.y());
            }
        } else {
            if ( scaled() ) {
                m_sx = (qreal)width() / (qreal)surfacePixmap()->width();
                m_sy = (qreal)height() / (qreal)surfacePixmap()->height();
                m_surfaceRect.setWidth(m_surfaceRect.width() * m_sx);
                m_surfaceRect.setHeight(m_surfaceRect.height() * m_sy);
                m_surfaceRect.moveCenter(rect().center());
                m_transform = QTransform::fromScale(1.0 / m_sx, 1.0 / m_sy);
            } else {
                m_sx = m_sy = 1.0;
                m_surfaceRect.moveCenter(rect().center());
                m_transform = QTransform::fromScale(1, 1);
                m_transform.translate(-m_surfaceRect.x(), -m_surfaceRect.y());
            }
        }
    }
    if ( e )
        QFrame::resizeEvent(e);
}

void SurfaceWidget::paintEvent(QPaintEvent *)
{
    if ( client() ) {
        m_painter.begin(surfacePixmap());
        for (int i = 0; i < connectionWindow()->updatePixmaps(client()).count(); i++)
            m_painter.drawPixmap(connectionWindow()->updateRects(client())[i].topLeft(), connectionWindow()->updatePixmaps(client())[i]);
        m_painter.end();
        connectionWindow()->updatePixmaps(client()).clear();
        connectionWindow()->updateRects(client()).clear();
        m_painter.begin(this);
        if ( bilinearFilter() )
            m_painter.setRenderHints(QPainter::SmoothPixmapTransform);
        m_painter.fillRect(rect(), m_backgroundBrush);
        if ( scaled() ) {
            if ( keepAspect() ) {
                m_surfaceRect.moveCenter(rect().center());
                m_painter.scale(m_scale, m_scale);
                m_painter.drawPixmap(m_surfaceRect.x() / m_scale, m_surfaceRect.y() / m_scale, *surfacePixmap());
            } else {
                m_painter.scale(m_sx, m_sy);
                m_painter.drawPixmap(0, 0, *surfacePixmap());
            }
        } else {
            m_painter.scale(1.0, 1.0);
            m_painter.drawPixmap((width() - surfacePixmap()->width()) / 2, (height() - surfacePixmap()->height()) / 2, *surfacePixmap());
        }
        m_painter.end();
        if ( showFps() ) {
            QPainterPath pp;
            m_painter.begin(this);
            m_painter.setPen(m_foregroundBrush.color());
            m_painter.setRenderHints(QPainter::TextAntialiasing | QPainter::Antialiasing | QPainter::HighQualityAntialiasing);
            m_painter.setFont(m_font);
            m_fpsMessage = tr("FPS: %1").arg(currentFps());
            m_textBoundingRect = m_painter.boundingRect(rect(), Qt::AlignHCenter | Qt::AlignVCenter, m_fpsMessage);
            m_textBackgroundSize = m_textBoundingRect.size();
            m_textBackgroundSize.scale(m_textBoundingRect.width() + 5, m_textBoundingRect.height() + 5, Qt::KeepAspectRatioByExpanding);
            m_textBackgroundRect = m_textBoundingRect;
            m_textBackgroundRect.setSize(m_textBackgroundSize);
            m_textBackgroundRect.moveBottom(rect().bottom());
            pp.addRoundedRect(m_textBackgroundRect, 5, 5);
            m_textBoundingRect.moveCenter(m_textBackgroundRect.center());
            m_painter.fillPath(pp, textBackgroundBrush());
            m_painter.drawText(m_textBoundingRect, m_fpsMessage);
            m_painter.end();
        }
    } else {
        m_painter.begin(this);
        m_painter.fillRect(rect(), backgroundBrush());
        m_painter.setPen(foregroundBrush().color());
        m_painter.setFont(m_font);
        m_textBoundingRect = m_painter.boundingRect(rect(), Qt::AlignHCenter | Qt::AlignVCenter, defaultMessage());
        m_textBoundingRect.moveCenter(rect().center());
        m_painter.drawText(m_textBoundingRect, defaultMessage());
        m_painter.end();
    }
    incFrameCounter();
}
