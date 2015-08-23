#include "phonescreen.h"
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

void PhoneScreen::on_applicationStateChanged(Qt::ApplicationState appState)
{
    mPhoneScreenThread->setAppState(appState);
}

void PhoneScreen::phoneScreenUpdate()
{
    QImage screen("t1wrench-screen.png");
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
    } else if (ev->type() == QEvent::Expose) {
        QExposeEvent *eev = (QExposeEvent *) ev;
    }
    return QDialog::eventFilter(obj, ev);
}

void PhoneScreen::resizeEvent(QResizeEvent *)
{

}
