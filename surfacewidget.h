#ifndef SURFACEWIDGET_H
#define SURFACEWIDGET_H

#include <QFrame>
#include <QPixmap>
#include <QPainter>
#include <QBrush>
#include <QRect>
#include <QTransform>
#include <QFont>
#include <QPoint>
#include <QString>
#include <QTimer>
#include <QFont>
#include <QRect>
#include <QSize>
#include <rfb/rfbclient.h>

class ConnectionWindow;

class SurfaceWidget : public QFrame
{
    Q_OBJECT

public:
    explicit SurfaceWidget(ConnectionWindow *connectionWindow, QWidget *parent);
    ~SurfaceWidget();

    QPixmap *surfacePixmap() { return m_surfacePixmap; }
    void setSurfaceSize(QSize surfaceSize);
    QSize surfaceSize() { return m_surfacePixmap ? m_surfacePixmap->size() : QSize(); }
    void setBackgroundBrush(QBrush brush) { m_backgroundBrush = brush; }
    QBrush backgroundBrush() { return m_backgroundBrush; }
    void setForegroundBrush(QBrush brush) { m_foregroundBrush = brush; }
    QBrush foregroundBrush() { return m_foregroundBrush; }
    void setTextBackgroundBrush(QBrush brush) { m_textBackgroundBrush = brush; }
    QBrush textBackgroundBrush() { return m_textBackgroundBrush; }
    bool bilinearFilter() { return scaled() && m_bilinearFilter; }
    void setBilinearFilter(bool bilinearFilter) { m_bilinearFilter = bilinearFilter; }
    bool scaled() { return m_scaled; }
    void setScaled(bool scaled) { m_scaled = scaled; }
    bool keepAspect() { return m_keepAspect; }
    void setKeepAspect(bool keep) { m_keepAspect = keep; }
    bool showFps() { return m_showFps; }
    void setShowFps(bool enable) { m_showFps = enable; }
    void setDefaultMessage(QString message) { m_defaultMessage = message; }
    QString &defaultMessage() { return m_defaultMessage; }
    int currentFps() { return m_currentFps; }
    void setCurrentFps(int fps) { m_currentFps = fps; }
    int frameCounter() { return m_frameCounter; }
    void incFrameCounter() { m_frameCounter++; }
    void setFrameCounter(int counter) { m_frameCounter = counter; }

    int translateMouseButton(Qt::MouseButton button);

    inline rfbClient *client();
    inline ConnectionWindow *connectionWindow();

public slots:
    void updateSurface();
    void clearSurface();
    void frameTimerTimeout();

protected:
    bool eventFilter(QObject *object, QEvent *event);
    void resizeEvent(QResizeEvent *e);
    void paintEvent(QPaintEvent *);

private:
    ConnectionWindow *m_connectionWindow;
    QPixmap *m_surfacePixmap;
    QBrush m_backgroundBrush;
    QBrush m_foregroundBrush;
    QBrush m_textBackgroundBrush;
    qreal m_scale, m_sx, m_sy;
    QPainter m_painter;
    QRect m_surfaceRect;
    QTransform m_transform;
    bool m_bilinearFilter;
    bool m_scaled;
    bool m_keepAspect;
    bool m_showFps;
    int m_buttonMask;
    QString m_defaultMessage;
    uint m_frameCounter;
    uint m_currentFps;
    QTimer m_frameTimer;
    QFont m_font;
    QRect m_textBoundingRect;
    QRect m_textBackgroundRect;
    QSize m_textBackgroundSize;
    QString m_fpsMessage;
};

#include "connectionwindow.h"

#endif // SURFACEWIDGET_H
