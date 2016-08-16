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
#include "mainwindow.h"

extern QtVncViewerSettings *globalConfig;
extern MainWindow *mainWindow;

bool ConnectionWindow::m_quiet = false;
quint64 ConnectionWindow::m_nextConnectionNumber = 0;
QHash<rfbClient *, QList<QPixmap> > ConnectionWindow::m_updatePixmaps;
QHash<rfbClient *, QList<QRect> > ConnectionWindow::m_updateRects;
QHash<rfbClient *, ConnectionWindow *> ConnectionWindow::m_clientToWindowHash;
QMutex PollServerThread::m_mutex;
bool PollServerThread::m_connecting = false;

ConnectionWindow::ConnectionWindow(QString name, QWidget *parent) :
    QMdiSubWindow(parent),
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
    layout()->addWidget(ui->frameConnectionMenu);
    if ( m_nextConnectionNumber == 0 ) {
        if ( QVNCVIEWER_ARG_RASTER )
            setSurfaceType(QVNCVIEWER_ARG_RASTER);
        if ( QVNCVIEWER_ARG_OPENGL )
            setSurfaceType(QVNCVIEWER_ARG_OPENGL);
    }
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
    if ( name.isEmpty() )
        name = tr("Disconnected-%1").arg(m_nextConnectionNumber++);
    m_defaultWindowTitle = name;
    setWindowTitle(m_defaultWindowTitle);

    // configuration menu
    m_configurationMenu = new QMenu(ui->toolButtonSettings);
    m_toggleScaledAction = m_configurationMenu->addAction(tr("Scaled display"), this, SLOT(toggleScaledAction_triggered(bool)));
    m_toggleScaledAction->setCheckable(true);
    m_toggleScaledAction->setChecked(globalConfig->preferencesScaled());
    m_toggleScaledAction->setToolTip(tr("Scale the display to the connection window size (recommended)?"));
    m_toggleScaledAction->setStatusTip(m_toggleScaledAction->toolTip());
    m_toggleBilinearFilterAction = m_configurationMenu->addAction(tr("Bilinear filtering"), this, SLOT(toggleBilinearFilterAction_triggered(bool)));
    m_toggleBilinearFilterAction->setCheckable(true);
    m_toggleBilinearFilterAction->setChecked(globalConfig->preferencesBilinearFilter());
    m_toggleBilinearFilterAction->setToolTip(tr("Use bilinear filtering on scaled displays?"));
    m_toggleBilinearFilterAction->setStatusTip(m_toggleBilinearFilterAction->toolTip());
    m_toggleKeepAspectAction = m_configurationMenu->addAction(tr("Keep aspect"), this, SLOT(toggleKeepAspectAction_triggered(bool)));
    m_toggleKeepAspectAction->setCheckable(true);
    m_toggleKeepAspectAction->setChecked(globalConfig->preferencesKeepAspect());
    m_toggleKeepAspectAction->setToolTip(tr("Keep aspect ratio on scaled displays?"));
    m_toggleKeepAspectAction->setStatusTip(m_toggleKeepAspectAction->toolTip());
    m_showFpsAction = m_configurationMenu->addAction(tr("Show FPS"), this, SLOT(showFpsAction_triggered(bool)));
    m_showFpsAction->setCheckable(true);
    m_showFpsAction->setChecked(globalConfig->preferencesShowFps());
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setShowFps(globalConfig->preferencesShowFps());
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setShowFps(globalConfig->preferencesShowFps());
        break;
    }
    m_showFpsAction->setToolTip(tr("Show number of frames per second?"));
    m_showFpsAction->setStatusTip(m_showFpsAction->toolTip());
    m_configurationMenu->addSeparator();
    m_takeScreenShotAction = m_configurationMenu->addAction(QIcon(":/images/camera.png"), tr("Save screen shot to file"), this, SLOT(takeScreenShotAction_triggered(bool)));
    m_takeScreenShotAction->setToolTip(tr("Take a shot of the current screen contents and save it to a file"));
    m_takeScreenShotAction->setStatusTip(m_takeScreenShotAction->toolTip());
    m_takeScreenShotToClipboardAction = m_configurationMenu->addAction(QIcon(":/images/camera.png"), tr("Copy screen shot to clipboard"), this, SLOT(takeScreenShotToClipboardAction_triggered(bool)));
    m_takeScreenShotToClipboardAction->setToolTip(tr("Take a shot of the current screen contents and copy it to the clipboard"));
    m_takeScreenShotToClipboardAction->setStatusTip(m_takeScreenShotToClipboardAction->toolTip());
    m_configurationMenu->addSeparator();
    m_encodingsMenu = new QMenu(m_configurationMenu);
    m_encodingsMenu->setIcon(QIcon(":/images/encoding.png"));
    if ( !MainWindow::encodings().contains(globalConfig->preferencesEncoding()) )
        globalConfig->setPreferencesEncoding("Hextile");
    m_currentEncoding = globalConfig->preferencesEncoding();
    foreach (QString encoding, MainWindow::encodings()) {
        QAction *action = m_encodingsMenu->addAction(encoding, this, SLOT(encodingChangedAction_triggered(bool)));
        action->setCheckable(true);
        if ( m_currentEncoding == encoding )
            action->setChecked(true);
    }
    m_encodingsMenu->setTitle(tr("Encoding '%1'").arg(m_currentEncoding));
    m_configurationMenu->addMenu(m_encodingsMenu);
    m_configurationMenu->addSeparator();
    m_qualitySlider = new RangeSlider(m_configurationMenu);
    m_qualitySlider->slider()->setMinimum(0);
    m_qualitySlider->slider()->setMaximum(9);
    m_qualitySlider->slider()->setTickInterval(1);
    m_qualitySlider->slider()->setValue(globalConfig->preferencesQualityLevel());
    m_qualitySlider->lcd()->display(globalConfig->preferencesQualityLevel());
    m_qualitySlider->slider()->setToolTip(tr("Set quality level (0 - 9)"));
    m_qualitySlider->slider()->setFocusPolicy(Qt::NoFocus);
    m_qualitySlider->label()->setText(tr("Quality"));
    m_qualitySliderAction = new QWidgetAction(m_configurationMenu);
    m_qualitySliderAction->setDefaultWidget(m_qualitySlider);
    connect(m_qualitySlider->slider(), SIGNAL(valueChanged(int)), this, SLOT(qualityLevelChanged(int)));
    m_configurationMenu->addAction(m_qualitySliderAction);
    m_compressionSlider = new RangeSlider(m_configurationMenu);
    m_compressionSlider->slider()->setMinimum(0);
    m_compressionSlider->slider()->setMaximum(9);
    m_compressionSlider->slider()->setTickInterval(1);
    m_compressionSlider->slider()->setValue(globalConfig->preferencesCompressionLevel());
    m_compressionSlider->lcd()->display(globalConfig->preferencesCompressionLevel());
    m_compressionSlider->slider()->setToolTip(tr("Set compression level (0 - 9)"));
    m_compressionSlider->slider()->setFocusPolicy(Qt::NoFocus);
    m_compressionSlider->label()->setText(tr("Compression"));
    m_compressionSliderAction = new QWidgetAction(m_configurationMenu);
    m_compressionSliderAction->setDefaultWidget(m_compressionSlider);
    connect(m_compressionSlider->slider(), SIGNAL(valueChanged(int)), this, SLOT(compressionLevelChanged(int)));
    m_configurationMenu->addAction(m_compressionSliderAction);
    ui->toolButtonSettings->setMenu(m_configurationMenu);
    connect(m_configurationMenu, SIGNAL(aboutToShow()), this, SLOT(configurationMenu_aboutToShow()));
    connect(m_configurationMenu, SIGNAL(aboutToHide()), this, SLOT(configurationMenu_aboutToHide()));

    connect(&m_menuFrameTimer, SIGNAL(timeout()), this, SLOT(menuBarTimer_timeout()));
    connect(&m_hostEditTimer, SIGNAL(timeout()), this, SLOT(hostEditTimer_timeout()));
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
    if ( m_configurationMenu )
        delete m_configurationMenu;
    delete ui;
}

void ConnectionWindow::setConnected(bool conn)
{
     m_connected = conn;
     if ( connected() ) {
         ui->lineEditHostname->setEnabled(false);
         ui->spinBoxDisplay->setEnabled(false);
         ui->toolButtonConnect->setIcon(QIcon(":/images/connected.png"));
         ui->toolButtonConnect->setToolTip(tr("Disconnect from VNC server"));
         ui->toolButtonConnect->setStatusTip(ui->toolButtonConnect->toolTip());
         m_pollServerThread = new PollServerThread(m_rfbClient, this);
         connect(pollServerThread(), SIGNAL(messageArrived()), this, SLOT(messageArrived()));
         connect(pollServerThread(), SIGNAL(connectionClosed()), this, SLOT(connectionClosed()));
         mainWindow->addRecentConnection(ui->lineEditHostname->text(), ui->spinBoxDisplay->value());
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
         ui->lineEditHostname->setEnabled(true);
         ui->spinBoxDisplay->setEnabled(true);
         ui->toolButtonConnect->setIcon(QIcon(":/images/disconnected.png"));
         ui->toolButtonConnect->setToolTip(tr("Connect to VNC server"));
         ui->toolButtonConnect->setStatusTip(ui->toolButtonConnect->toolTip());
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
    ui->frameConnectionMenu->setVisible(enable);
}

bool ConnectionWindow::menuFrameVisible()
{
    return ui->frameConnectionMenu->isVisible();
}

void ConnectionWindow::startSessionFromArguments(QString hostName, int displayNumber, bool fullScreen, bool maximize)
{
    if ( maximize )
        showMaximized();
    if ( fullScreen )
        on_toolButtonToggleFullScreen_clicked();
    ui->lineEditHostname->blockSignals(true);
    ui->lineEditHostname->setText(hostName);
    ui->lineEditHostname->blockSignals(false);
    ui->spinBoxDisplay->setValue(displayNumber);
    ui->toolButtonConnect->setEnabled(true);
    QTimer::singleShot(0, ui->toolButtonConnect, SLOT(animateClick()));
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
        MainWindow::log(message);
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
    MainWindow::log(tr("Disconnected from %1:%2").arg(ui->lineEditHostname->text()).arg(ui->spinBoxDisplay->value()));
}

void ConnectionWindow::hostNameLookedUp(const QHostInfo &host)
{
    if ( host.error() == QHostInfo::NoError )
        ui->toolButtonConnect->setEnabled(!host.addresses().isEmpty());
    else
        ui->toolButtonConnect->setEnabled(false);
    m_lookUpId = -1;
}

void ConnectionWindow::configurationMenu_aboutToShow()
{
    int w = qMax(m_qualitySlider->slider()->width(), m_compressionSlider->slider()->width());
    m_qualitySlider->slider()->setFixedWidth(w);
    m_compressionSlider->slider()->setFixedWidth(w);
}

void ConnectionWindow::configurationMenu_aboutToHide()
{
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setFocus();
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setFocus();
        break;
    }
}

void ConnectionWindow::on_toolButtonConnect_clicked()
{
    ui->toolButtonConnect->setEnabled(false);
    if ( connected() )
        doDisconnect();
    else {
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
        if ( ConnectToRFBServer(m_rfbClient, ui->lineEditHostname->text().toLocal8Bit().constData(), ui->spinBoxDisplay->value() + QVNCVIEWER_VNC_BASE_PORT) ) {
            MainWindow::log(tr("Connected to %1:%2").arg(ui->lineEditHostname->text()).arg(ui->spinBoxDisplay->value()));
            PollServerThread::setConnecting(false);
            if ( InitialiseRFBConnection(m_rfbClient) ) {
                QString rfbDesktopName(m_rfbClient->desktopName);
                if ( !rfbDesktopName.isEmpty() )
                    setWindowTitle(rfbDesktopName);
                MainWindow::log(tr("Setting encoding to '%1'").arg(m_currentEncoding));
                MainWindow::log(tr("Setting quality level to '%1'").arg(m_qualitySlider->slider()->value()));
                MainWindow::log(tr("Setting compression level to '%1'").arg(m_compressionSlider->slider()->value()));
                m_rfbClient->appData.encodingsString = (const char*)m_currentEncoding.constData();
                m_rfbClient->appData.qualityLevel = m_qualitySlider->slider()->value();
                m_rfbClient->appData.compressLevel = 9 - m_compressionSlider->slider()->value();
                if ( !SetFormatAndEncodings(m_rfbClient) )
                    MainWindow::log(tr("WARNING: Failed sending formats and encondings to %1:%2").arg(ui->lineEditHostname->text()).arg(ui->spinBoxDisplay->value()));
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
                MainWindow::log(tr("WARNING: Failed RFB client initialization for %1:%2").arg(ui->lineEditHostname->text()).arg(ui->spinBoxDisplay->value()));
            }
        } else {
            PollServerThread::setConnecting(false);
            setConnected(false);
            rfbClientCleanup(m_rfbClient);
            m_clientToWindowHash.remove(m_rfbClient);
            m_rfbClient = 0;
            setWindowTitle(m_defaultWindowTitle);
            MainWindow::log(tr("WARNING: Failed connection to %1:%2").arg(ui->lineEditHostname->text()).arg(ui->spinBoxDisplay->value()));
        }
    }
    ui->toolButtonConnect->setEnabled(true);
}

void ConnectionWindow::on_toolButtonToggleFullScreen_clicked()
{
    if ( isFullScreen() ) {
        if ( surfaceType() == QVNCVIEWER_SURFACE_RASTER ) {
            surfaceWidget()->setFrameShadow(QFrame::Raised);
            surfaceWidget()->setFrameShape(QFrame::StyledPanel);
        }
#if QT_VERSION >= 0x050000
        showNormal();
        setParent(mainWindow->mdiArea());
#else
        mainWindow->mdiArea()->addSubWindow(this);
#endif
        ui->frameConnectionMenu->setVisible(true);
        mainWindow->setFullScreenWindow(0);
        foreach (QMdiSubWindow *w, mainWindow->mdiArea()->subWindowList())
            w->showMaximized();
    } else {
        mainWindow->mdiArea()->removeSubWindow(this);
        setParent(0);
        if ( surfaceType() == QVNCVIEWER_SURFACE_RASTER ) {
            surfaceWidget()->setFrameShadow(QFrame::Plain);
            surfaceWidget()->setFrameShape(QFrame::NoFrame);
        }
        ui->frameConnectionMenu->setVisible(false);
        showFullScreen();
        mainWindow->setFullScreenWindow(this);
    }
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setFocus();
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setFocus();
        break;
    }
}

void ConnectionWindow::on_lineEditHostname_textChanged(const QString &)
{
    ui->toolButtonConnect->setEnabled(false);
    m_hostEditTimer.start(QVNCVIEWER_HOSTEDIT_TIMEOUT);
}

void ConnectionWindow::hostEditTimer_timeout()
{
    m_hostEditTimer.stop();
    if ( m_lookUpId >= 0 )
        QHostInfo::abortHostLookup(m_lookUpId);
    m_lookUpId = QHostInfo::lookupHost(ui->lineEditHostname->text(), this, SLOT(hostNameLookedUp(QHostInfo)));
}

void ConnectionWindow::menuBarTimer_timeout()
{
    m_menuFrameTimer.stop();
    if ( isFullScreen() ) {
        if ( m_configurationMenu->isActiveWindow() || QCursor::pos().y() < ui->frameConnectionMenu->height() + layout()->margin() )
            m_menuFrameTimer.start(QVNCVIEWER_MENUBAR_TIMEOUT);
        else {
            if ( surfaceType() == QVNCVIEWER_SURFACE_RASTER ) {
                surfaceWidget()->setFrameShadow(QFrame::Plain);
                surfaceWidget()->setFrameShape(QFrame::NoFrame);
            }
            ui->frameConnectionMenu->setVisible(false);
        }
    }
}

void ConnectionWindow::toggleScaledAction_triggered(bool checked)
{
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setScaled(checked);
        surfaceWidgetGL()->setFocus();
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setScaled(checked);
        surfaceWidget()->setFocus();
        break;
    }
    QTimer::singleShot(0, surfaceWidget(), SLOT(updateSurface()));
}

void ConnectionWindow::toggleBilinearFilterAction_triggered(bool checked)
{
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setBilinearFilter(checked);
        surfaceWidgetGL()->setFocus();
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setBilinearFilter(checked);
        surfaceWidget()->setFocus();
        break;
    }
    QTimer::singleShot(0, surfaceWidget(), SLOT(updateSurface()));
}

void ConnectionWindow::toggleKeepAspectAction_triggered(bool checked)
{
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setKeepAspect(checked);
        surfaceWidgetGL()->setFocus();
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setKeepAspect(checked);
        surfaceWidget()->setFocus();
        break;
    }
    QTimer::singleShot(0, surfaceWidget(), SLOT(updateSurface()));
}

void ConnectionWindow::showFpsAction_triggered(bool checked)
{
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setShowFps(checked);
        surfaceWidgetGL()->setFocus();
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setShowFps(checked);
        surfaceWidget()->setFocus();
        break;
    }
    QTimer::singleShot(0, surfaceWidget(), SLOT(updateSurface()));
}

void ConnectionWindow::takeScreenShotAction_triggered(bool checked)
{
    QPixmap screenShot;
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        screenShot = *surfaceWidgetGL()->surfacePixmap();
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        screenShot = *surfaceWidget()->surfacePixmap();
        break;
    }
    QString fileName = QFileDialog::getSaveFileName(this, tr("Choose screen shot file"), QString(), QString(), 0, globalConfig->preferencesNativeFileDialogs() ? (QFileDialog::Options)0 : QFileDialog::DontUseNativeDialog);
    if ( !fileName.isEmpty() ) {
        QFileInfo fi(fileName);
        if ( fi.suffix().isEmpty() )
            fileName += ".png";
        screenShot.save(fileName);
    }
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setFocus();
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setFocus();
        break;
    }
}

void ConnectionWindow::takeScreenShotToClipboardAction_triggered(bool checked)
{
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        qApp->clipboard()->setPixmap(*surfaceWidgetGL()->surfacePixmap());
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        qApp->clipboard()->setPixmap(*surfaceWidget()->surfacePixmap());
        break;
    }
}

void ConnectionWindow::qualityLevelChanged(int qualityLevel)
{
    if ( m_rfbClient ) {
        m_rfbClient->appData.encodingsString = (const char*)m_currentEncoding.constData();
        m_rfbClient->appData.qualityLevel = qualityLevel;
        m_rfbClient->appData.compressLevel = 9 - m_compressionSlider->slider()->value();
        if ( connected() ) {
            MainWindow::log(tr("Setting quality level to '%1'").arg(qualityLevel));
            PollServerThread::mutex().lock();
            pollServerThread()->setCheckConnection(true);
            if ( !SetFormatAndEncodings(m_rfbClient) )
                MainWindow::log(tr("WARNING: Failed sending formats and encondings to %1:%2").arg(ui->lineEditHostname->text()).arg(ui->spinBoxDisplay->value()));
            m_rfbClient->updateRect.x = m_rfbClient->updateRect.y = 0;
            m_rfbClient->updateRect.w = m_rfbClient->width;
            m_rfbClient->updateRect.h = m_rfbClient->height;
            SendIncrementalFramebufferUpdateRequest(m_rfbClient);
            PollServerThread::mutex().unlock();
        }
    }
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setFocus();
        QTimer::singleShot(0, surfaceWidgetGL(), SLOT(updateSurface()));
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setFocus();
        QTimer::singleShot(0, surfaceWidget(), SLOT(updateSurface()));
        break;
    }
}

void ConnectionWindow::compressionLevelChanged(int compressionLevel)
{
    if ( m_rfbClient ) {
        m_rfbClient->appData.encodingsString = (const char*)m_currentEncoding.constData();
        m_rfbClient->appData.qualityLevel = m_qualitySlider->slider()->value();
        m_rfbClient->appData.compressLevel = 9 - compressionLevel;
        if ( connected() ) {
            MainWindow::log(tr("Setting compression level to '%1'").arg(compressionLevel));
            PollServerThread::mutex().lock();
            pollServerThread()->setCheckConnection(true);
            if ( !SetFormatAndEncodings(m_rfbClient) )
                MainWindow::log(tr("WARNING: Failed sending formats and encondings to %1:%2").arg(ui->lineEditHostname->text()).arg(ui->spinBoxDisplay->value()));
            m_rfbClient->updateRect.x = m_rfbClient->updateRect.y = 0;
            m_rfbClient->updateRect.w = m_rfbClient->width;
            m_rfbClient->updateRect.h = m_rfbClient->height;
            SendIncrementalFramebufferUpdateRequest(m_rfbClient);
            PollServerThread::mutex().unlock();
        }
    }
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->setFocus();
        QTimer::singleShot(0, surfaceWidgetGL(), SLOT(updateSurface()));
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setFocus();
        QTimer::singleShot(0, surfaceWidget(), SLOT(updateSurface()));
        break;
    }
}

void ConnectionWindow::encodingChangedAction_triggered(bool checked)
{
    QAction *action = (QAction *)sender();
    m_currentEncoding = action->text();
    m_encodingsMenu->setTitle(tr("Encoding '%1'").arg(m_currentEncoding));
    foreach (QAction *a, m_encodingsMenu->actions())
        a->setChecked(a == action);
    if ( m_rfbClient ) {
        m_rfbClient->appData.encodingsString = (const char*)m_currentEncoding.constData();;
        m_rfbClient->appData.qualityLevel = m_qualitySlider->slider()->value();
        m_rfbClient->appData.compressLevel = 9 - m_compressionSlider->slider()->value();
        if ( connected() ) {
            MainWindow::log(tr("Setting encoding to '%1'").arg(m_currentEncoding));
            PollServerThread::mutex().lock();
            pollServerThread()->setCheckConnection(true);
            if ( !SetFormatAndEncodings(m_rfbClient) )
                MainWindow::log(tr("WARNING: Failed sending formats and encondings to %1:%2").arg(ui->lineEditHostname->text()).arg(ui->spinBoxDisplay->value()));
            m_rfbClient->updateRect.x = m_rfbClient->updateRect.y = 0;
            m_rfbClient->updateRect.w = m_rfbClient->width;
            m_rfbClient->updateRect.h = m_rfbClient->height;
            SendIncrementalFramebufferUpdateRequest(m_rfbClient);
            PollServerThread::mutex().unlock();
        }
    }
    switch ( surfaceType() ) {
    case QVNCVIEWER_SURFACE_OPENGL:
        surfaceWidgetGL()->updateGL();
        surfaceWidgetGL()->setFocus();
        QTimer::singleShot(0, surfaceWidgetGL(), SLOT(updateSurface()));
        break;
    case QVNCVIEWER_SURFACE_RASTER:
    default:
        surfaceWidget()->setFocus();
        QTimer::singleShot(0, surfaceWidget(), SLOT(updateSurface()));
        break;
    }
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
