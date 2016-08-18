#ifndef CONNECTIONWINDOW_H
#define CONNECTIONWINDOW_H

#include <QElapsedTimer>
#include <QWidgetAction>
#include <QCloseEvent>
#include <QString>
#include <QTimer>
#include <QThread>
#include <QMutex>
#include <QHash>
#include <QList>
#include <QPixmap>
#include <QRect>
#include <QHostInfo>
#include <QAction>
#include <QMenu>
#include <rfb/rfbclient.h>

#include "surfacewidget.h"
#include "surfacewidget_gl.h"
#include "rangeslider.h"
#include <QGridLayout>

namespace Ui {
class ConnectionWindow;
}

class PollServerThread : public QThread
{
    Q_OBJECT

public:
    explicit PollServerThread(rfbClient *client, QObject *parent = 0);
    void setExit(bool e) { m_exit = e; }
    bool checkConnection() { return m_checkConnection; }
    void setCheckConnection(bool check) { m_checkConnection = check; }

    static QMutex &mutex() { return m_mutex; }
    static void setConnecting(bool connecting) { m_connecting = connecting; }
    static bool connecting() { return m_connecting; }

signals:
    void messageArrived();
    void connectionClosed();

protected:
    void run();

private:
    rfbClient *m_rfbClient;
    bool m_exit;
    bool m_checkConnection;
    QElapsedTimer m_lastMessageReceivedTimer;
    static QMutex m_mutex;
    static bool m_connecting;
};

class ConnectionWindow : public QWidget
{
    Q_OBJECT

    private:
    QString mHostName;
    int mDisplayNumber;

public:
    explicit ConnectionWindow(QWidget *parent = 0);
    ~ConnectionWindow();

    static void setQuiet(bool quiet) { m_quiet = quiet; }
    static bool quiet() { return m_quiet; }

    bool connected() { return m_connected; }
    void setConnected(bool conn);
    void setMenuFrameVisible(bool enable);
    bool menuFrameVisible();
    void setSurfaceType(int surfaceType) { m_surfaceType = surfaceType; }
    int surfaceType() { return m_surfaceType; }

    QTimer &menuFrameTimer() { return m_menuFrameTimer; }
    void startSessionFromArguments(QString hostName, int displayNumber, bool fullScreen, bool maximize);

    SurfaceWidget *surfaceWidget() { return m_surfaceWidget; }
    SurfaceWidgetGL *surfaceWidgetGL() { return m_surfaceWidgetGL; }
    PollServerThread *pollServerThread() { return m_pollServerThread; }
    rfbClient *rfbClientPtr() { return m_rfbClient; }

    static QList<QPixmap> &updatePixmaps(rfbClient *client);
    static QList<QRect> &updateRects(rfbClient *client);
    static ConnectionWindow *clientWindow(rfbClient *client);

    // RFB related
    static rfbBool rfbResize(rfbClient *client);
    static void rfbUpdate(rfbClient *client,int x, int y, int w, int h);
    /* FIXME: we currently don't need these functions
    static void rfbUpdateCopyRect(rfbClient *, int, int, int, int, int, int);
    static void rfbUpdateFinished(rfbClient *);
    static rfbBool rfbHandleCursorPos(rfbClient *, int, int);
    */
    static void rfbLog(const char *format, ...);

public slots:
    // callbacks
    void doConnect();
    void doDisconnect();
    void hostNameLookedUp(const QHostInfo &host);
    void configurationMenu_aboutToShow();
    void configurationMenu_aboutToHide();

    // for signals from poll thread
    void messageArrived();
    void connectionClosed();

signals:
    void windowClosed();

protected:
    void closeEvent(QCloseEvent *event);

private:
    QGridLayout mLayout;
    static bool m_quiet;
    static quint64 m_nextConnectionNumber;
    static QHash<rfbClient *, QList<QPixmap> > m_updatePixmaps;
    static QHash<rfbClient *, QList<QRect> > m_updateRects;
    static QHash<rfbClient *, ConnectionWindow *> m_clientToWindowHash;
    Ui::ConnectionWindow *ui;
    QTimer m_menuFrameTimer;
    QTimer m_hostEditTimer;
    rfbClient *m_rfbClient;
    bool m_connected;
    QString m_defaultWindowTitle;
    PollServerThread *m_pollServerThread;
    SurfaceWidget *m_surfaceWidget;
    SurfaceWidgetGL *m_surfaceWidgetGL;
    int m_lookUpId;
    QString m_currentEncoding;
    int m_surfaceType;
};

#endif // CONNECTIONWINDOW_H
