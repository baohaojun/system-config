#include <QPainterPath>

#include "surfacewidget_gl.h"
#include "qvncviewersettings.h"
#include "qt2keysum.h"
#include "macros.h"

extern QtVncViewerSettings *globalConfig;

SurfaceWidgetGL::SurfaceWidgetGL(ConnectionWindow *connectionWindow, QWidget *parent)
    : QGLWidget(QGLFormat(QGL::SampleBuffers | QGL::DoubleBuffer), parent),
      m_connectionWindow(connectionWindow),
      m_scale(1.0),
      m_sx(1.0),
      m_sy(1.0),
      m_bilinearFilter(true),
      m_scaled(true),
      m_keepAspect(true),
      m_showFps(false),
      m_buttonMask(0),
      m_textureId(-1),
      m_bgTextureId(-1),
      m_frameCounter(0),
      m_currentFps(0)
{
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

SurfaceWidgetGL::~SurfaceWidgetGL()
{
}

void SurfaceWidgetGL::initializeGL()
{
    glClearColor(backgroundBrush().color().red(), backgroundBrush().color().green(), backgroundBrush().color().blue(), backgroundBrush().color().alpha());
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glDisable(GL_DEPTH);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

void SurfaceWidgetGL::paintGL()
{
    glClear(GL_COLOR_BUFFER_BIT);
    if ( client() ) {
        m_painter.begin(surfacePixmap());
        for (int i = 0; i < connectionWindow()->updatePixmaps(client()).count(); i++)
            m_painter.drawPixmap(connectionWindow()->updateRects(client())[i].topLeft(), connectionWindow()->updatePixmaps(client())[i]);
        m_painter.end();
        connectionWindow()->updatePixmaps(client()).clear();
        connectionWindow()->updateRects(client()).clear();
        if ( m_textureId >= 0 )
            deleteTexture(m_textureId);
        if ( bilinearFilter() ) {
            m_textureId = bindTexture(m_surfacePixmap, GL_TEXTURE_2D, GL_RGBA, QGLContext::DefaultBindOption);
            // this avoids filtering artifacts at texture edges
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        } else
            m_textureId = bindTexture(m_surfacePixmap, GL_TEXTURE_2D, GL_RGBA, QGLContext::InvertedYBindOption);
        if ( scaled() ) {
            m_textureRect = rect();
            if ( keepAspect() ) {
                m_textureRect.setWidth(surfacePixmap()->width() * m_scale);
                m_textureRect.setHeight(surfacePixmap()->height() * m_scale);
            }
        } else
            m_textureRect = surfacePixmap()->rect();
        m_textureRect.moveCenter(rect().center());
        drawTexture(m_textureRect, m_textureId);
        if ( showFps() ) {
            QPainterPath pp;
            QPixmap dummyPixmap(size());
            m_fpsMessage = tr("FPS: %1").arg(currentFps());
            m_painter.begin(&dummyPixmap);
            m_painter.setFont(m_font);
            m_textBoundingRect = m_painter.boundingRect(rect(), Qt::AlignHCenter | Qt::AlignVCenter, m_fpsMessage);
            m_painter.end();
            m_textBackgroundSize = m_textBoundingRect.size();
            m_textBackgroundSize.scale(m_textBoundingRect.width() + 5, m_textBoundingRect.height() + 5, Qt::KeepAspectRatioByExpanding);
            m_textBackgroundRect = m_textBoundingRect;
            m_textBackgroundRect.setSize(m_textBackgroundSize);
            m_textBackgroundRect.moveBottom(rect().bottom());
            if ( m_bgTextureId >= 0 )
                deleteTexture(m_bgTextureId);
            m_bgPixmap = QPixmap(m_textBackgroundRect.size());
            m_bgPixmap.fill(Qt::transparent);
            m_painter.begin(&m_bgPixmap);
            m_painter.setRenderHints(QPainter::Antialiasing | QPainter::HighQualityAntialiasing);
            pp.addRoundedRect(m_bgPixmap.rect(), 5, 5);
            m_painter.fillPath(pp, textBackgroundBrush());
            m_painter.end();
            m_textBoundingRect.moveCenter(m_textBackgroundRect.center());
            m_bgTextureId = bindTexture(m_bgPixmap, GL_TEXTURE_2D, GL_RGBA, QGLContext::DefaultBindOption);
            glPushMatrix();
            drawTexture(m_textBackgroundRect, m_bgTextureId);
            qglColor(foregroundBrush().color());
            renderText(m_textBoundingRect.bottomLeft().x(), m_textBackgroundRect.bottom() - m_textBoundingRect.height()/4, 0, m_fpsMessage, m_font);
            glPopMatrix();
        }
    } else {
        QPixmap dummyPixmap(size());
        m_painter.begin(&dummyPixmap);
        m_painter.setFont(m_font);
        m_textBoundingRect = m_painter.boundingRect(rect(), Qt::AlignHCenter | Qt::AlignVCenter, defaultMessage());
        m_painter.end();
        m_textBoundingRect.moveCenter(rect().center());
        glPushMatrix();
        qglColor(foregroundBrush().color());
        renderText(m_textBoundingRect.topLeft().x(), m_textBoundingRect.topLeft().y(), 0, defaultMessage(), m_font);
        glPopMatrix();
    }
    incFrameCounter();
}

void SurfaceWidgetGL::resizeGL(int w, int h)
{
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0, w, h, 0, -1, 1);
    glMatrixMode(GL_MODELVIEW);
}

void SurfaceWidgetGL::setSurfaceSize(QSize surfaceSize)
{
    m_surfacePixmap = QPixmap(surfaceSize);
    m_surfacePixmap.fill(backgroundBrush().color());
    m_surfaceRect = surfacePixmap()->rect();
    m_surfaceRect.setWidth(m_surfaceRect.width() * m_scale);
    m_surfaceRect.setHeight(m_surfaceRect.height() * m_scale);
    m_transform = QTransform::fromScale(1.0 / m_scale, 1.0 / m_scale);
    QTimer::singleShot(0, this, SLOT(updateSurface()));
}

int SurfaceWidgetGL::translateMouseButton(Qt::MouseButton button)
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

rfbClient *SurfaceWidgetGL::client()
{
    return connectionWindow()->rfbClientPtr();
}

ConnectionWindow *SurfaceWidgetGL::connectionWindow()
{
    return m_connectionWindow;
}

void SurfaceWidgetGL::updateSurface()
{
    m_font = font();
    m_font.setPointSize(m_font.pointSize() * 2);
    m_font.setBold(true);
    resizeEvent(0);
    update();
}

void SurfaceWidgetGL::clearSurface()
{
    setSurfaceSize(surfacePixmap()->size());
    setCurrentFps(0);
    setFrameCounter(0);
}

void SurfaceWidgetGL::frameTimerTimeout()
{
    setCurrentFps(frameCounter());
    setFrameCounter(0);
    m_frameTimer.start(QVNCVIEWER_ONE_SECOND);
}

bool SurfaceWidgetGL::eventFilter(QObject *object, QEvent *event)
{
    switch ( event->type() ) {
    case QEvent::MouseMove: {
            QMouseEvent *mouseEvent = static_cast<QMouseEvent *>(event);
            if ( mouseEvent->pos().y() < QVNCVIEWER_MENUBAR_MOUSE_Y ) {
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
    return QGLWidget::eventFilter(object, event);
}

void SurfaceWidgetGL::resizeEvent(QResizeEvent *e)
{
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
    if ( e )
        QGLWidget::resizeEvent(e);
}
