#include <QFileDialog>
#include <QApplication>
#include <QClipboard>
#include <QFileInfo>
#include <QMouseEvent>
#include <QCursor>
#include <QImage>
#include <QIcon>
#include <QTest>

#include "connectionwindow.h"
#include "ui_connectionwindow.h"
#include "qvncviewersettings.h"
#include "vncmainwindow.h"

extern QtVncViewerSettings *globalConfig;
extern VncMainWindow *vncMainWindow;

bool ConnectionWindow::m_quiet = false;
quint64 ConnectionWindow::m_nextConnectionNumber = 0;
QHash<rfbClient *, QList<QPixmap> > ConnectionWindow::m_updatePixmaps;
QHash<rfbClient *, QList<QRect> > ConnectionWindow::m_updateRects;
QHash<rfbClient *, ConnectionWindow *> ConnectionWindow::m_clientToWindowHash;
QMutex PollServerThread::m_mutex;
bool PollServerThread::m_connecting = false;

ConnectionWindow::ConnectionWindow(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::ConnectionWindow),
    m_connected(false),
    m_rfbClient(0),
    m_pollServerThread(0),
    m_surfaceWidget(0),
    m_surfaceWidgetGL(0),
    m_lookUpId(-1),
    m_surfaceType(globalConfig->preferencesSurfaceType())
{
    ui->setupUi(this);
    if ( m_nextConnectionNumber == 0 ) {
        if ( QVNCVIEWER_ARG_RASTER )
            setSurfaceType(QVNCVIEWER_ARG_RASTER);
        if ( QVNCVIEWER_ARG_OPENGL )
            setSurfaceType(QVNCVIEWER_ARG_OPENGL);
    }
    this->setLayout(&mLayout);
    layout()->setContentsMargins(0, 0, 0, 0);
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        m_surfaceWidgetGL = new SurfaceWidgetGL(this, this);
        layout()->addWidget(surfaceWidgetGL());
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        m_surfaceWidget = new SurfaceWidget(this, this);
        layout()->addWidget(surfaceWidget());
        break;
    }
    layout()->setSpacing(0);

    // configuration menu
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setShowFps(globalConfig->preferencesShowFps());
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setShowFps(globalConfig->preferencesShowFps());
        break;
    }
    m_currentEncoding = globalConfig->preferencesEncoding();
}

ConnectionWindow::~ConnectionWindow()
{
    if ( m_rfbClient ) {
        if ( m_rfbClient->frameBuffer )
            free(m_rfbClient->frameBuffer);
        rfbClientCleanup(m_rfbClient);
    }
    if ( pollServerThread() ) {
        pollServerThread()->setExit(true);
        pollServerThread()->wait();
        delete pollServerThread();
    }
    if ( surfaceWidget() )
        delete surfaceWidget();
    if ( surfaceWidgetGL() )
        delete surfaceWidgetGL();
    delete ui;
}

void ConnectionWindow::setConnected(bool conn)
{
     m_connected = conn;
     if ( connected() ) {
         m_pollServerThread = new PollServerThread(m_rfbClient, this);
         connect(pollServerThread(), SIGNAL(messageArrived()), this, SLOT(messageArrived()));
         connect(pollServerThread(), SIGNAL(connectionClosed()), this, SLOT(connectionClosed()));
         switch ( surfaceType() ) {
         case QVNCVIEWER_SURFACE_OPENGL:
             surfaceWidgetGL()->setSurfaceSize(QSize(m_rfbClient->width, m_rfbClient->height));
             surfaceWidgetGL()->setDefaultMessage(tr("Connected"));
             surfaceWidgetGL()->update();
             surfaceWidgetGL()->setFocus();
             break;
         case QVNCVIEWER_SURFACE_RASTER:
         default:
             surfaceWidget()->setSurfaceSize(QSize(m_rfbClient->width, m_rfbClient->height));
             surfaceWidget()->setDefaultMessage(tr("Connected"));
             surfaceWidget()->update();
             surfaceWidget()->setFocus();
             break;
         }
         pollServerThread()->start();
         m_rfbClient->updateRect.x = m_rfbClient->updateRect.y = 0;
         m_rfbClient->updateRect.w = m_rfbClient->width;
         m_rfbClient->updateRect.h = m_rfbClient->height;
         SendIncrementalFramebufferUpdateRequest(m_rfbClient);
     } else {
         if ( pollServerThread() ) {
             pollServerThread()->setExit(true);
             pollServerThread()->wait();
             delete pollServerThread();
             m_pollServerThread = 0;
         }
         switch ( surfaceType() ) {
         case QVNCVIEWER_SURFACE_OPENGL:
             surfaceWidgetGL()->clearSurface();
             surfaceWidgetGL()->setDefaultMessage(tr("Disconnected"));
             surfaceWidgetGL()->update();
             break;
         case QVNCVIEWER_SURFACE_RASTER:
         default:
             surfaceWidget()->clearSurface();
             surfaceWidget()->setDefaultMessage(tr("Disconnected"));
             surfaceWidget()->update();
             break;
         }
     }
}

void ConnectionWindow::setMenuFrameVisible(bool enable)
{
}

bool ConnectionWindow::menuFrameVisible()
{
}

void ConnectionWindow::startSessionFromArguments(QString hostName, int displayNumber, bool fullScreen, bool maximize)
{
    if ( maximize )
        showMaximized();
    mHostName = hostName;
    mDisplayNumber = displayNumber;
    QTimer::singleShot(1000, this, SLOT(doConnect()));
}

QList<QPixmap> &ConnectionWindow::updatePixmaps(rfbClient *client)
{
    static QList<QPixmap> nullPixmaps;

    if ( m_updatePixmaps.contains(client) )
        return m_updatePixmaps[client];
    else
        return nullPixmaps;
}

QList<QRect> &ConnectionWindow::updateRects(rfbClient *client)
{
    static QList<QRect> nullRects;

    if ( m_updateRects.contains(client) )
        return m_updateRects[client];
    else
        return nullRects;
}

ConnectionWindow *ConnectionWindow::clientWindow(rfbClient *client)
{
    if ( m_clientToWindowHash.contains(client) )
        return m_clientToWindowHash[client];
    else
        return 0;
}

rfbBool ConnectionWindow::rfbResize(rfbClient *client)
{
    ConnectionWindow *connectionWindow = clientWindow(client);
    if ( connectionWindow ) {
        uint8_t *oldFB = client->frameBuffer;
        client->frameBuffer = (uint8_t *)malloc(client->width * client->height * QVNCVIEWER_BYTES_PER_PIXEL);
        free(oldFB);
        switch ( connectionWindow->surfaceType() ) {
        case QVNCVIEWER_SURFACE_OPENGL:
            connectionWindow->surfaceWidgetGL()->setSurfaceSize(QSize(client->width, client->height));
            break;
        case QVNCVIEWER_SURFACE_RASTER:
        default:
            connectionWindow->surfaceWidget()->setSurfaceSize(QSize(client->width, client->height));
            break;
        }
        return true;
    } else
        return false;
}

void ConnectionWindow::rfbUpdate(rfbClient *client, int x, int y, int w, int h)
{
    QImage image = QImage(w, h, QImage::Format_ARGB32);
    for (int xx = x; xx < x + w; xx++) {
        for (int yy = y; yy < y + h; yy++) {
            qint32 pos = (yy * client->width * QVNCVIEWER_BYTES_PER_PIXEL) + (xx * QVNCVIEWER_BYTES_PER_PIXEL);
            image.setPixel(xx - x, yy - y, qRgb((quint8)client->frameBuffer[pos + 0], (quint8)client->frameBuffer[pos + 1], (quint8)client->frameBuffer[pos + 2]));
        }
    }
    m_updatePixmaps[client] << QPixmap::fromImage(image);
    m_updateRects[client] << QRect(x, y, w, h);
    ConnectionWindow *connectionWindow = clientWindow(client);
    if ( connectionWindow ) {
        switch ( connectionWindow->surfaceType() ) {
        case QVNCVIEWER_SURFACE_OPENGL:
            connectionWindow->surfaceWidgetGL()->update();
            break;
        case QVNCVIEWER_SURFACE_RASTER:
        default:
            connectionWindow->surfaceWidget()->update();
            break;
        }
    }
}

/* FIXME: we currently don't need these functions
void ConnectionWindow::rfbUpdateCopyRect(rfbClient *, int, int, int, int, int, int)
{
}

void ConnectionWindow::rfbUpdateFinished(rfbClient *)
{
}

bool ConnectionWindow::rfbHandleCursorPos(rfbClient *, int, int)
{
    return true;
}
*/

void ConnectionWindow::rfbLog(const char *format, ...)
{
    if ( !quiet() ) {
        va_list args;
        QString message;
        va_start(args, format);
        message.vsprintf(format, args);
        message.prepend(tr("RFB") + ": ");
        va_end(args);
        VncMainWindow::log(message);
    }
}

void ConnectionWindow::doDisconnect()
{
    setConnected(false);
    if ( m_rfbClient ) {
        ::close(m_rfbClient->sock);
        free(m_rfbClient->frameBuffer);
        m_rfbClient->frameBuffer = 0;
        rfbClientCleanup(m_rfbClient);
        m_updatePixmaps.remove(m_rfbClient);
        m_clientToWindowHash.remove(m_rfbClient);
        m_rfbClient = 0;
    }
    setWindowTitle(m_defaultWindowTitle);
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setDefaultMessage(tr("Disconnected"));
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setDefaultMessage(tr("Disconnected"));
        break;
    }
}

void ConnectionWindow::hostNameLookedUp(const QHostInfo &host)
{
    m_lookUpId = -1;
}

void ConnectionWindow::configurationMenu_aboutToShow()
{
}

void ConnectionWindow::configurationMenu_aboutToHide()
{
}

void ConnectionWindow::messageArrived()
{
    if ( pollServerThread() ) {
        PollServerThread::mutex().lock();
        if ( WaitForMessage(m_rfbClient, 0) > 0 )
            if ( !HandleRFBServerMessage(m_rfbClient) )
                QTimer::singleShot(0, this, SLOT(doDisconnect()));
        PollServerThread::mutex().unlock();
    }
}

void ConnectionWindow::connectionClosed()
{
    doDisconnect();
}

void ConnectionWindow::closeEvent(QCloseEvent *event)
{
    emit windowClosed();
    event->accept();
}

PollServerThread::PollServerThread(rfbClient *client, QObject *parent) :
    QThread(parent),
    m_rfbClient(client),
    m_exit(false),
    m_checkConnection(true)
{
}

void PollServerThread::run()
{
    while ( !m_exit ) {
        if ( !connecting() && mutex().tryLock(10) ) {
            int n = WaitForMessage(m_rfbClient, 500);
            mutex().unlock();
            if ( n < 0 ) {
                m_exit = true;
                emit connectionClosed();
            } else if ( n > 0 ) {
                emit messageArrived();
                m_lastMessageReceivedTimer.start();
            } else if ( checkConnection() ) {
                if ( ((ConnectionWindow *)parent())->connected() && m_lastMessageReceivedTimer.elapsed() > QVNCVIEWER_CONNPEND_TIMEOUT ) {
                    setCheckConnection(false);
                    m_rfbClient->updateRect.x = m_rfbClient->updateRect.y = 0;
                    m_rfbClient->updateRect.w = m_rfbClient->width;
                    m_rfbClient->updateRect.h = m_rfbClient->height;
                    SendIncrementalFramebufferUpdateRequest(m_rfbClient);
                }
            }
            QTest::qWait(0);
        } else  if ( connecting() ) {
            setCheckConnection(true);
            qApp->processEvents(QEventLoop::AllEvents, 10);
        }
    }
}

void ConnectionWindow::doConnect()
{
    if (connected())
        return;
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setDefaultMessage(tr("Connecting..."));
        surfaceWidgetGL()->update();
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setDefaultMessage(tr("Connecting..."));
        surfaceWidget()->update();
        break;
    }
    qApp->processEvents(QEventLoop::AllEvents, 100);
    // prepare RFB client structure
    m_rfbClient = rfbGetClient(QVNCVIEWER_BITS_PER_SAMPLE, QVNCVIEWER_SAMPLES_PER_PIXEL, QVNCVIEWER_BYTES_PER_PIXEL);
    m_rfbClient->MallocFrameBuffer = rfbResize;
    m_rfbClient->GotFrameBufferUpdate = rfbUpdate;
    /* FIXME: we currently don't need these functions
       m_rfbClient->FinishedFrameBufferUpdate = rfbUpdateFinished;
       m_rfbClient->GotCopyRect = rfbUpdateCopyRect;
       m_rfbClient->HandleCursorPos = rfbHandleCursorPos;
    */
    m_rfbClient->programName = QVNCVIEWER_APP_TITLE_CSTR;
    m_rfbClient->frameBuffer = 0;
    m_rfbClient->canHandleNewFBSize = true;
    m_rfbClient->canUseCoRRE = true;
    m_rfbClient->canUseHextile = true;
    m_rfbClient->appData.forceTrueColour = true;
    m_rfbClient->appData.useRemoteCursor = true;
    m_rfbClient->appData.enableJPEG = true;
    m_clientToWindowHash[m_rfbClient] = this;
    PollServerThread::setConnecting(true);
    if ( ConnectToRFBServer(m_rfbClient, mHostName.toLocal8Bit().constData(), mDisplayNumber + QVNCVIEWER_VNC_BASE_PORT) ) {
        PollServerThread::setConnecting(false);
        if ( InitialiseRFBConnection(m_rfbClient) ) {
            QString rfbDesktopName(m_rfbClient->desktopName);
            if ( !rfbDesktopName.isEmpty() )
                setWindowTitle(rfbDesktopName);
            VncMainWindow::log(tr("Setting encoding to '%1'").arg(m_currentEncoding));
            m_rfbClient->appData.encodingsString = (const char*)m_currentEncoding.constData();
            m_rfbClient->appData.qualityLevel = 95;
            m_rfbClient->appData.compressLevel = 9;
            if ( !SetFormatAndEncodings(m_rfbClient) )
                VncMainWindow::log(tr("WARNING: Failed sending formats and encondings to %1:%2").arg(mHostName).arg(mDisplayNumber));
            m_rfbClient->width = m_rfbClient->si.framebufferWidth;
            m_rfbClient->height = m_rfbClient->si.framebufferHeight;
            m_rfbClient->frameBuffer = (uint8_t *)malloc(m_rfbClient->width * m_rfbClient->height * QVNCVIEWER_BYTES_PER_PIXEL);
            setConnected(true);
        } else {
            PollServerThread::setConnecting(false);
            setConnected(false);
            ::close(m_rfbClient->sock);
            free(m_rfbClient->frameBuffer);
            m_rfbClient->frameBuffer = 0;
            rfbClientCleanup(m_rfbClient);
            m_clientToWindowHash.remove(m_rfbClient);
            m_rfbClient = 0;
            setWindowTitle(m_defaultWindowTitle);
            VncMainWindow::log(tr("WARNING: Failed RFB client initialization for %1:%2").arg(mHostName).arg(mDisplayNumber));
        }
    } else {
        PollServerThread::setConnecting(false);
        setConnected(false);
        rfbClientCleanup(m_rfbClient);
        m_clientToWindowHash.remove(m_rfbClient);
        m_rfbClient = 0;
        setWindowTitle(m_defaultWindowTitle);
        VncMainWindow::log(tr("WARNING: Failed connection to %1:%2").arg(mHostName).arg(mDisplayNumber));
    }
}
