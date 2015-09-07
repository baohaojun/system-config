#include "phonescreendialog.h"
#include "t1wrench.h"
#include "ui_phonescreendialog.h"
#include <QMouseEvent>
#include <QTime>
#include "adbphonescreenthread.hpp"
#include <QPixmap>
#include <QDebug>
#include "t1wrenchmainwindow.h"


PhoneScreenDialog::PhoneScreenDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::PhoneScreenDialog)
{
    isShelled = true;
    ui->setupUi(this);
    mT1Wrench = (T1WrenchMainWindow *)parent;
    mPhoneScreenThread = new AdbPhoneScreenThread(this);
    mPhoneScreenThread->start();
}

QSharedPointer<LuaExecuteThread> PhoneScreenDialog::mLuaThread()
{
    return mT1Wrench->mLuaThread;
}

void qSystem(QString str)
{
    system(str.toUtf8().constData());
}

void PhoneScreenDialog::phoneScreenUpdated()
{
    QString screenFile = "t1wrench-screen.png";
    if (gScreenCapJpg) {
        screenFile = "t1wrench-screen.jpg";
    }
    QImage screen(screenFile);
    if (screen.isNull()) {
        qDebug() << "using" << screenFile;
        system("bash -c 'pwd; ls -l t1wrench-screen.jpg'");
    }
    if (isShelled) {
        ui->phoneScreenLabel->setPixmap(QPixmap::fromImage(screen.scaled(this->width() * 1080 / 1187, this->height() * 1920 / 2457)));
    } else {
        ui->phoneScreenLabel->setPixmap(QPixmap::fromImage(screen.scaled(this->width(), this->height())));
    }

}

void PhoneScreenDialog::reloadBackground()
{
    int finalWidth;

    if (isShelled) {
        finalWidth = this->height() * 1187 / 2457;
    } else {
        finalWidth = this->height() * 1080 / 1920;
    }

    if (finalWidth != this->width()) {
        this->setFixedSize(finalWidth, this->height());
        return;
    }

    if (isShelled) {
        ui->phoneShellLabel->resize(this->size());
        ui->phoneShellLabel->move(0, 0);
        QString screenFile = "screenshot_shell.png";

        QImage screen(screenFile);
        if (screen.isNull()) {
            qDebug() << "using" << screenFile;
            system("bash -c 'pwd; ls -l t1wrench-screen.jpg'");
        }
        ui->phoneShellLabel->setPixmap(QPixmap::fromImage(screen.scaled(this->width(), this->height())));

        ui->phoneScreenLabel->resize(this->size().width() * 1080 / 1187, this->size().height() * 1920 / 2457);
        ui->phoneScreenLabel->move(this->size().width() * 53 / 1187, this->size().height() * 259 / 2457);
    } else {
        ui->phoneScreenLabel->resize(this->size());
        ui->phoneScreenLabel->move(0, 0);
    }
}

PhoneScreenDialog::~PhoneScreenDialog()
{
    mPhoneScreenThread->stopIt();
    mPhoneScreenThread->wait();
    delete mPhoneScreenThread;
    delete ui;
}

bool PhoneScreenDialog::eventFilter(QObject *obj, QEvent *ev)
{
    static int press_x, press_y;
    static QTime press_time;

    if (ev->type() == QEvent::MouseButtonPress) {
        QMouseEvent *mev = (QMouseEvent*) ev;
        press_x = mev->x() * 1080 / this->width();
        press_y = mev->y() * 1920 / this->height();
        press_time = QTime::currentTime();
    } else if (ev->type() == QEvent::MouseButtonRelease) {
        QMouseEvent *mev = (QMouseEvent*) ev;
        int x = mev->x() * 1080 / this->width();
        int y = mev->y() * 1920 / this->height();
        QTime now = QTime::currentTime();
        if (abs(x - press_x) + abs(y - press_y) > 20) {
            mLuaThread()->addScript(QStringList() << "adb_event" << QString().sprintf("adb-swipe-%d %d %d %d %d", now.msecsTo(press_time), press_x, press_y, x, y));
        } else {
            if (isShelled) {

            } else {
                mLuaThread()->addScript(QStringList() << "adb_event" << QString().sprintf("adb-tap %d %d", x, y));
            }
        }
        mPhoneScreenThread->syncScreen();
        return true;
    } else if (ev->type() == QEvent::Resize) {
        reloadBackground();
    } else if (ev->type() == QEvent::KeyPress) {
        QKeyEvent *kev = (QKeyEvent *)ev;
        int key = kev->key();
        Qt::KeyboardModifiers m = kev->modifiers();

        if (m == 0) {
            if (key == Qt::Key_F1) {
                isShelled = !isShelled;
                reloadBackground();
            } else if (key == Qt::Key_Home ) {
                mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key home");
                mPhoneScreenThread->syncScreen();
                return true;
            } else if (key == Qt::Key_Escape) {
                mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key back");
                mPhoneScreenThread->syncScreen();
                return true;
            } else if (key == Qt::Key_Pause) {
                mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key power");
                mPhoneScreenThread->syncScreen();
                return true;
            } else if (key == Qt::Key_Backspace) {
                mLuaThread()->addScript(QStringList() << "adb_event" << "adb-key DEL");
                mPhoneScreenThread->syncScreen();
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
                mPhoneScreenThread->syncScreen();
                return true;
            } else if (kev->text()[0].isPrint()) {
                mLuaThread()->addScript(QStringList() << "adb_event" << QString("adb-text ") + kev->text());
            }
            mPhoneScreenThread->syncScreen();
            return true;
        }
    } else if (ev->type() == QEvent::Wheel) {
        QWheelEvent *wev = (QWheelEvent *)ev;
        int y = wev->angleDelta().y();
        int x = wev->angleDelta().x();
        mLuaThread()->addScript(QStringList() << "adb_event" << QString().sprintf("adb-swipe-50 %d %d %d %d", wev->x(), wev->y(), wev->x() + x, wev->y() + y));
        mPhoneScreenThread->syncScreen();
    }
    return QDialog::eventFilter(obj, ev);
}

void PhoneScreenDialog::resizeEvent(QResizeEvent *)
{

}

void PhoneScreenDialog::showEvent(QShowEvent *ev)
{
    mPhoneScreenThread->continueLoop();
}

void PhoneScreenDialog::hideEvent(QHideEvent *ev)
{
    mPhoneScreenThread->pauseLoop();
}

void PhoneScreenDialog::closeEvent(QCloseEvent *event)
{
    mPhoneScreenThread->quit();
}

void PhoneScreenDialog::syncScreen()
{
    mPhoneScreenThread->syncScreen();
}
