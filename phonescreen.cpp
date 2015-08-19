#include "phonescreen.h"
#include "ui_phonescreen.h"
#include <QMouseEvent>
#include <QTime>
#include "adbphonescreen.hpp"
#include <QPixmap>


PhoneScreen::PhoneScreen(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::PhoneScreen)
{
    ui->setupUi(this);

    mPhoneScreenThread = QSharedPointer<AdbPhoneScreenThread>(new AdbPhoneScreenThread(this));
    connect(mPhoneScreenThread.data(), SIGNAL(phoneScreenUpdate()), this, SLOT(phoneScreenUpdate()));
    mPhoneScreenThread->start();
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
            system(QString().sprintf("set -x; the-true-adb shell input touchscreen swipe %d %d %d %d %d", press_x, press_y, x, y, now.msecsTo(press_time)).toUtf8().constData());
        } else {
            system(QString().sprintf("set -x; the-true-adb shell input tap %d %d", x, y).toUtf8().constData());
        }
    } else if (ev->type() == QEvent::Resize) {
        QResizeEvent *rev = (QResizeEvent *) ev;
        ui->phoneScreen->resize(rev->size());
        ui->phoneScreen->move(0, 0);
    }
    fprintf(stderr, "ev is %d", ev->type());
}

void PhoneScreen::resizeEvent(QResizeEvent *)
{

}
