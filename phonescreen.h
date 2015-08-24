#ifndef PHONESCREEN_H
#define PHONESCREEN_H

#include <QDialog>
#include "adbphonescreen.hpp"
#include <QSharedPointer>
#include <QWindow>

namespace Ui {
class PhoneScreen;
}

class PhoneScreen : public QDialog
{
    Q_OBJECT

public:
    explicit PhoneScreen(QWidget *parent = 0);
    ~PhoneScreen();

private slots:
    void phoneScreenUpdate();

protected:
    bool eventFilter(QObject *obj, QEvent *ev);
    void resizeEvent(QResizeEvent *);

private:
    Ui::PhoneScreen *ui;
    QSharedPointer<AdbPhoneScreenThread> mPhoneScreenThread;
    void showEvent(QShowEvent*);
    void hideEvent(QHideEvent*);
private slots:
    void on_applicationStateChanged(Qt::ApplicationState);
};

#endif // PHONESCREEN_H
