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
#include "wrenchext.h"
#include "wrench.h"
#include "bhj_help.hpp"

extern QtVncViewerSettings *globalConfig;

QStringList VncMainWindow::m_encodings;

void VncMainWindow::initFromWrenchExt()
{
    WrenchExt wrenchExt;

    QString phoneWidth = wrenchExt.getConfig("phone-width");
    QString phoneHeight = wrenchExt.getConfig("phone-height");
    QString wheelScale = wrenchExt.getConfig("wheel-scale");
    QString wheelTime = wrenchExt.getConfig("wheel-time");
    if (wrenchExt.getConfig("allow-vnc-resize") == "true") {
        mAllowResize = true;
    } else {
        mAllowResize = false;
    }

    mPhoneWidth = phoneWidth.toInt();
    mPhoneWidth = mPhoneWidth ? mPhoneWidth : 1080;
    mPhoneHeight = phoneHeight.toInt();
    mPhoneHeight = mPhoneHeight ? mPhoneHeight : 1920;
    mWheelScale = wheelScale.toInt();
    mWheelScale = mWheelScale ? mWheelScale : 1;
    mWheelTime = wheelTime.toInt();
    mWheelTime = mWheelTime ? mWheelTime : 100;

    if (mAllowResize) {
        setFixedSize(QSize(QWIDGETSIZE_MAX, QWIDGETSIZE_MAX));
        resize(parentWidget()->size().height() * mPhoneWidth / mPhoneHeight, parentWidget()->size().height());
    } else {
        setFixedSize(parentWidget()->size().height() * 1080 / 1920, parentWidget()->size().height());
    }
    this->activateWindow();
}

VncMainWindow::VncMainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::VncMainWindow)
{
    ui->setupUi(this);

    initFromWrenchExt();
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
    QStringList lastArgumentList;
    lastArgumentList << "127.0.0.1" << QString("%1").arg(1 + QProcessEnvironment::systemEnvironment().value("WRENCH_INSTANCE", "0").toInt());
    if ( lastArgumentList.count() == 2 ) {
        QString hostName = lastArgumentList[0];
        int displayNumber = lastArgumentList[1].toInt();
        ui->connectionWindow->startSessionFromArguments(hostName, displayNumber, QVNCVIEWER_ARG_FULLSCREEN, QVNCVIEWER_ARG_MAXIMIZE);
    }
}

void VncMainWindow::loadSettings()
{
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
    static bool isTouchDown;

    if (ev->type() == QEvent::Resize) {
        QResizeEvent *rev = (QResizeEvent *)ev;
        QSize size = rev->size();
        if (size.height() * mPhoneWidth / mPhoneHeight != size.width()) {
            size.rwidth() = size.height() * mPhoneWidth / mPhoneHeight;
            this->resize(size);
            return true;
        }
    } else if (ev->type() == QEvent::MouseButtonPress) {
        QMouseEvent *mev = (QMouseEvent*) ev;
        if (mev->button() != Qt::LeftButton) {
            return false;
        }

        isTouchDown = true;
        last_x = mev->x();
        last_y = mev->y();
        phone_x = mev->x() * mPhoneWidth / this->width();
        phone_y = mev->y() * mPhoneHeight / this->height();
        press_time = QTime::currentTime();
        mLuaThread()->addScript(QStringList() << "adb_event" << QString().sprintf("adb-no-virt-key-down %d %d", phone_x, phone_y));
    } else if (ev->type() == QEvent::MouseButtonRelease) {
        QMouseEvent *mev = (QMouseEvent*) ev;
        int x = mev->x() * mPhoneWidth / this->width();
        int y = mev->y() * mPhoneHeight / this->height();
        QTime now = QTime::currentTime();

        if (mev->button() == Qt::RightButton) {
            mLuaThread()->addScript(QStringList() << "start_or_stop_recording");
            return true;
        }

        QStringList command("adb_event");
        if (mev->button() != Qt::LeftButton) {
            command = QStringList() << "log", "%s";
        }

        isTouchDown = false;

        int old_x = phone_x;
        int old_y = phone_y;

        mLuaThread()->addScript(QStringList() << command << QString().sprintf("adb-no-virt-key-up %d %d", x, y));
        return true;
    } else if (ev->type() == QEvent::MouseMove) {
        if (isTouchDown) {
            static int last_x, last_y;


            QMouseEvent *mev = (QMouseEvent*) ev;
            int x = mev->x() * mPhoneWidth / this->width();
            int y = mev->y() * mPhoneHeight / this->height();
            if (last_x && last_y && abs(x - last_x) + abs(y - last_y) > 10)
                mLuaThread()->addScript(QStringList() << "adb_event" << QString().sprintf("adb-no-virt-key-move %d %d", x, y));
            last_x = x;
            last_y = y;
            return true;
        }
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
            } else if (key == Qt::Key_F12) {
                mLuaThread()->addScript(QStringList() << "call_ext" << "reconnect-vnc");
            } else if (key == Qt::Key_Pause) {
                mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key power");
                return true;
            } else if (key == Qt::Key_Backspace) {
                mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key DEL");
                return true;
            } else if (key == Qt::Key_PageDown || key == Qt::Key_PageUp) {
                QString func = (key == Qt::Key_PageDown) ? "vnc_page_down" : "vnc_page_up";
                mLuaThread()->addScript(QStringList() << func);
            }
        }
        if (key == Qt::Key_Left || key == Qt::Key_Right ||
            key == Qt::Key_Down || key == Qt::Key_Up) {
            QString scroll_key =
                (key == Qt::Key_Left) ? "left" :
                (key == Qt::Key_Right) ? "right" :
                (key == Qt::Key_Down) ? "down" :
                (key == Qt::Key_Up) ? "up" : "unknown";

            QString scroll_mod =
                (m & Qt::ControlModifier) ? "control" :
                (m & Qt::AltModifier) ? "alt" :
                (m & Qt::ShiftModifier) ? "shift" : "";

            mLuaThread()->addScript(QStringList() << "vnc_scroll" << scroll_key << scroll_mod);
        }

        if (!kev->text().isEmpty()) {

            if (key == Qt::Key_Enter || key == Qt::Key_Return) {
                if (m == Qt::ControlModifier) {
                    mLuaThread()->addScript(QStringList() << "wrench_send_action");
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
        QWheelEvent *mev = (QWheelEvent *)ev;
        int dy = mev->angleDelta().y() * mWheelScale;
        int dx = mev->angleDelta().x() * mWheelScale;

        int x = mev->x() * mPhoneWidth / this->width();
        int y = mev->y() * mPhoneHeight / this->height();

        int new_x = qMax(x + dx, 0);
        int new_y = qMax(y + dy, 0);

        if (new_y > mPhoneHeight) {
            new_y = mPhoneHeight;
        }

        if (new_x > mPhoneWidth) {
            new_x = mPhoneWidth;
        }

        QString event = QString().sprintf("adb-no-virt-key-swipe-%d %d %d %d %d", mWheelTime, x, y, new_x, new_y);
        mLuaThread()->addScript(QStringList() << "adb_event" << event);
    }
    return QMainWindow::eventFilter(object, ev);
}

void VncMainWindow::closeEvent(QCloseEvent *e)
{

    if (isWrenchQuitting()) {
        saveSettings();
        e->accept();
    } else {
        fprintf(stderr, "%s:%d: hide\n", __FILE__, __LINE__);
        hide();
        e->ignore();
    }
    // QTimer::singleShot(0, qApp, SLOT(quit()));
}

void VncMainWindow::hideEvent(QHideEvent *e)
{
    gPhoneScreenSyncOn = false;
    mLuaThread()->addScript(QStringList("kill_android_vnc"));

    ui->connectionWindow->doDisconnect();
    emit vncWindowClosed();
}

void VncMainWindow::showEvent(QShowEvent *e)
{
    gPhoneScreenSyncOn = true;
    initFromWrenchExt();
    QTimer::singleShot(10, ui->connectionWindow, SLOT(doConnect()));
}

void VncMainWindow::onVncUpdate(QString state) {
    if (this->isVisible() && ! ui->connectionWindow->connected()) {
        QTimer::singleShot(0, ui->connectionWindow, SLOT(doConnect()));
    }

    if (state == "Online") {
        initFromWrenchExt();
    }
}
