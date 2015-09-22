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
    isShelled = false;
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
            system("bash -c 'pwd; ls -l screenshot_shell.png'");
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
            mLuaThread()->addScript(QStringList() << "adb_event" << QString().sprintf("adb-swipe-%d %d %d %d %d", now.msecsTo(press_time), phone_x, phone_y, x, y));
        } else {
            if (isShelled) {
                static int phonePowerButtonX = 953;
                static int phonePowerButtonY = 8;

                static int phoneLeftUPX = 6;
                static int phoneLeftUPY = 1128;
                static int phoneLeftDownX = 6;
                static int phoneLeftDownY = 1332;

                static int phoneRightUpX = 1178;
                static int phoneRightUpY = phoneLeftUPY;
                static int phoneRightDownX = phoneRightUpX;
                static int phoneRightDownY = phoneLeftDownY;

                static int phoneMenuX = 354;
                static int phoneHomeX = 594;
                static int phoneBackX = 834;
                static int phone3KeysY = 2300;

                static int phoneWidth = 1178;
                static int phoneHeight = 2457;

                int phoneLastX = last_x * phoneWidth / this->width();
                int phoneLastY = last_y * phoneHeight / this->height();

                static int phoneScreenLeftX = 52;
                static int phoneScreenRightX = 1132;

                static int phoneScreenUpY = 258;
                static int phoneScreenDownY = phoneScreenUpY + 1920;

                if (phoneLastX < phoneScreenRightX &&
                    phoneLastX > phoneScreenLeftX &&
                    phoneLastY < phoneScreenDownY &&
                    phoneLastY > phoneScreenUpY) {

                    int x = (phoneLastX - phoneScreenLeftX);
                    int y = (phoneLastY - phoneScreenUpY);

                    mLuaThread()->addScript(QStringList() << "adb_event" << QString().sprintf("adb-tap %d %d", x, y));
                    mPhoneScreenThread->syncScreen();
                    return true;
                }

                static struct {
                    int x;
                    int y;
                    const char *key;
                } keyLocationMap[] = {
                    { phonePowerButtonX, phonePowerButtonY, "power" },
                    { phoneLeftUPX, phoneLeftUPY, "volume_up" },
                    { phoneLeftDownX, phoneLeftDownY, "volume_down" },
                    { phoneRightUpX, phoneRightUpY, "volume_up" },
                    { phoneRightDownX, phoneRightDownY, "volume_down" },
                    { phoneMenuX, phone3KeysY, "menu" },
                    { phoneHomeX, phone3KeysY, "home" },
                    { phoneBackX, phone3KeysY, "back" },
                };

                for (int i = 0; i < sizeof keyLocationMap / sizeof keyLocationMap[0]; i++) {
                    if (abs(phoneLastX - keyLocationMap[i].x) + abs(phoneLastY - keyLocationMap[i].y) < 100) {
                        mLuaThread()->addScript(QStringList() << "adb_event" << QString("adb-key ") + keyLocationMap[i].key);
                        mPhoneScreenThread->syncScreen();
                        return true;
                    }
                }
            } else {
                mLuaThread()->addScript(QStringList() << "adb_event" << QString().sprintf("adb-tap %d %d", x, y));
            }
        }
        mPhoneScreenThread->syncScreen();
        return true;
    } else if (ev->type() == QEvent::Show) {
        reloadBackground();
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
