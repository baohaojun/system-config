#include <QDesktopServices>
#include <QApplication>
#include <QKeySequence>
#include <QDateTime>
#include <QShortcut>
#include <QWidget>
#include <QAction>
#include <QTimer>
#include <QUrl>
#include <stdio.h>

#include "macros.h"
#include "vncmainwindow.h"
#include "qvncviewersettings.h"
#include "aboutdialog.h"
#include "wrenchmainwindow.h"

extern QtVncViewerSettings *globalConfig;

QStringList VncMainWindow::m_encodings;

VncMainWindow::VncMainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::VncMainWindow)
{
    ui->setupUi(this);
    centralWidget()->layout()->setContentsMargins(0, 0, 0, 0);
    m_fullScreenWindow = 0;
    mWrench = (WrenchMainWindow *)parent;
    if ( encodings().isEmpty() ) {
        encodings() << "Raw" << "Hextile" << "Tight" << "CoRRE" << "Zlib" << "Ultra";
        encodings().sort();
    }
    loadSettings();
    installEventFilter(this);
    QTimer::singleShot(0, this, SLOT(init()));
    rfbClientLog = rfbClientErr = ConnectionWindow::rfbLog;
}

QSharedPointer<LuaExecuteThread> VncMainWindow::mLuaThread()
{
    return mWrench->mLuaThread;
}

VncMainWindow::~VncMainWindow()
{
    delete ui;
}

void VncMainWindow::log(QString message)
{
    message.prepend(QTime::currentTime().toString("hh:mm:ss.zzz") + ": ");
    printf("%s\n", message.trimmed().toLocal8Bit().constData());
    fflush(stdout);
}


void VncMainWindow::init()
{
    if ( QVNCVIEWER_ARG_QUIET )
        ConnectionWindow::setQuiet(true);
    m_recentConnections = globalConfig->mainWindowRecentConnections();
    QTimer::singleShot(0, this, SLOT(initClientFromArguments()));
}

void VncMainWindow::initClientFromArguments()
{
    QStringList lastArgumentList = {"localhost", "1"};
    if ( lastArgumentList.count() == 2 ) {
        QString hostName = lastArgumentList[0];
        int displayNumber = lastArgumentList[1].toInt();
        ui->connectionWindow->startSessionFromArguments(hostName, displayNumber, QVNCVIEWER_ARG_FULLSCREEN, QVNCVIEWER_ARG_MAXIMIZE);
    }
}

void VncMainWindow::loadSettings()
{
    restoreGeometry(globalConfig->mainWindowGeometry());
    restoreState(globalConfig->mainWindowState());
}

void VncMainWindow::saveSettings()
{
    globalConfig->setMainWindowGeometry(saveGeometry());
    globalConfig->setMainWindowState(saveState());
}

bool VncMainWindow::eventFilter(QObject *object, QEvent *ev)
{
    static int phone_x, phone_y;
    static QTime press_time;

    static int last_x, last_y;

    if (ev->type() == QEvent::MouseButtonPress) {
        QMouseEvent *mev = (QMouseEvent*) ev;
        last_x = mev->x();
        last_y = mev->y();
        phone_x = mev->x() * 1080 / this->width();
        phone_y = mev->y() * 1920 / this->height();
        press_time = QTime::currentTime();
    } else if (ev->type() == QEvent::MouseButtonRelease) {
        QMouseEvent *mev = (QMouseEvent*) ev;
        int x = mev->x() * 1080 / this->width();
        int y = mev->y() * 1920 / this->height();
        QTime now = QTime::currentTime();
        if (abs(x - phone_x) + abs(y - phone_y) > 20) {
            mLuaThread()->addScript(QStringList() << "adb_event" << QString().sprintf("adb-no-virt-key-swipe-%d %d %d %d %d", now.msecsTo(press_time), phone_x, phone_y, x, y));
        } else {
            mLuaThread()->addScript(QStringList() << "adb_event" << QString().sprintf("adb-no-virt-key-tap %d %d", x, y));
        }
        return true;
    } else if (ev->type() == QEvent::KeyPress) {
        QKeyEvent *kev = (QKeyEvent *)ev;
        int key = kev->key();
        Qt::KeyboardModifiers m = kev->modifiers();

        if (m == 0) {
            if (key == Qt::Key_Home ) {
                mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key home");
                return true;
            } else if (key == Qt::Key_Escape) {
                mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key back");
                return true;
            } else if (key == Qt::Key_Pause) {
                mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key power");
                return true;
            } else if (key == Qt::Key_Backspace) {
                mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key DEL");
                return true;
            }
        }

        if (!kev->text().isEmpty()) {

            if (key == Qt::Key_Enter || key == Qt::Key_Return) {
                if (m == Qt::ControlModifier) {
                    mLuaThread()->addScript(QStringList() << "t1_send_action");
                } else if (m == 0) {
                    mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key enter");
                }
            } else if (key == Qt::Key_Space || key == Qt::Key_Tab) {
                mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key space");
            } else if (key == Qt::Key_Backspace) {
                mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key DEL");
                return true;
            } else if (kev->text()[0].isPrint()) {
                mLuaThread()->addScript(QStringList() << "adb_event" << QString("adb-text ") + kev->text());
            }
            return true;
        }
    } else if (ev->type() == QEvent::Wheel) {
        QWheelEvent *wev = (QWheelEvent *)ev;
        int y = wev->angleDelta().y();
        int x = wev->angleDelta().x();
        mLuaThread()->addScript(QStringList() << "adb_event" << QString().sprintf("adb-no-virt-key-swipe-50 %d %d %d %d", wev->x(), wev->y(), wev->x() + x, wev->y() + y));
    }
    return QMainWindow::eventFilter(object, ev);
}

void VncMainWindow::closeEvent(QCloseEvent *e)
{
    saveSettings();
    e->accept();
    // QTimer::singleShot(0, qApp, SLOT(quit()));
}
