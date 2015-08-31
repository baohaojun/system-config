#include "phonescreen.h"
#include "t1wrench.h"
#include "ui_phonescreen.h"
#include <QMouseEvent>
#include <QTime>
#include "adbphonescreen.hpp"
#include <QPixmap>
#include <QDebug>


PhoneScreen::PhoneScreen(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::PhoneScreen)
{
    ui->setupUi(this);

    mPhoneScreenThread = QSharedPointer<AdbPhoneScreenThread>(new AdbPhoneScreenThread(this));
    connect(mPhoneScreenThread.data(), SIGNAL(phoneScreenUpdate()), this, SLOT(phoneScreenUpdate()));
    mPhoneScreenThread->start();
    connect(qApp, SIGNAL(applicationStateChanged(Qt::ApplicationState)), this, SLOT(on_applicationStateChanged(Qt::ApplicationState)));
}

void qSystem(QString str)
{
    system(str.toUtf8().constData());
}

void PhoneScreen::on_applicationStateChanged(Qt::ApplicationState appState)
{
    mPhoneScreenThread->setAppState(appState);
}

void PhoneScreen::phoneScreenUpdate()
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
    ui->phoneScreen->setPixmap(QPixmap::fromImage(screen.scaled(this->width(), this->height())));
}

PhoneScreen::~PhoneScreen()
{
    mPhoneScreenThread->stopIt();
    mPhoneScreenThread->wait();
    mPhoneScreenThread.clear();
    delete ui;
}

bool PhoneScreen::eventFilter(QObject *obj, QEvent *ev)
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
            system(QString().sprintf("the-true-adb shell input touchscreen swipe %d %d %d %d %d", press_x, press_y, x, y, now.msecsTo(press_time)).toUtf8().constData());
        } else {
            system(QString().sprintf("the-true-adb shell input tap %d %d", x, y).toUtf8().constData());
        }
        return true;
    } else if (ev->type() == QEvent::Resize) {
        QResizeEvent *rev = (QResizeEvent *) ev;
        ui->phoneScreen->resize(rev->size());
        ui->phoneScreen->move(0, 0);
    } else if (ev->type() == QEvent::KeyPress) {
        QKeyEvent *kev = (QKeyEvent *)ev;
        int key = kev->key();
        Qt::KeyboardModifiers m = kev->modifiers();

        if (m == 0) {
            if (key == Qt::Key_Home ) {
                system("the-true-adb shell input keyevent HOME");
                return true;
            } else if (key == Qt::Key_Escape) {
                system("the-true-adb shell input keyevent BACK");
                return true;
            } else if (key == Qt::Key_Pause) {
                system("the-true-adb shell input keyevent POWER");
                return true;
            } else if (key == Qt::Key_Backspace) {
                system("the-true-adb shell input keyevent DEL");
            }
        }

        if (!kev->text().isEmpty()) {
            qDebug() << "key is" << kev->text();
            if (kev->text() == "'") {
                system("the-true-adb shell input text \"'\"");
            } else if (kev->text() == "?") {
                system("the-true-adb shell input text '\\?'");
            } else {
                system(QString().sprintf("the-true-adb shell input text \"'%s'\"", kev->text().toUtf8().constData()).toUtf8().constData());
            }
            return true;
        }
    } else if (ev->type() == QEvent::Wheel) {
        QWheelEvent *wev = (QWheelEvent *)ev;
        int y = wev->angleDelta().y();
        int x = wev->angleDelta().x();
        qSystem(QString().sprintf("the-true-adb shell input touchscreen swipe %d %d %d %d 50", wev->x(), wev->y(), wev->x() + x, wev->y() + y));
    }
    return QDialog::eventFilter(obj, ev);
}

void PhoneScreen::resizeEvent(QResizeEvent *)
{

}

void PhoneScreen::showEvent(QShowEvent *ev)
{
    mPhoneScreenThread->continueLoop();
}

void PhoneScreen::hideEvent(QHideEvent *ev)
{
    mPhoneScreenThread->pauseLoop();
}
