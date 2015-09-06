#ifndef PHONESCREEN_H
#define PHONESCREEN_H

#include <QDialog>
#include "adbphonescreenthread.hpp"
#include <QSharedPointer>
#include <QWindow>

namespace Ui {
class PhoneScreenDialog;
}

class PhoneScreenDialog : public QDialog
{
    Q_OBJECT

public:
    explicit PhoneScreenDialog(QWidget *parent = 0);
    ~PhoneScreenDialog();

private slots:
    void phoneScreenUpdate();

protected:
    bool eventFilter(QObject *obj, QEvent *ev);
    void resizeEvent(QResizeEvent *);

private:
    Ui::PhoneScreenDialog *ui;
    QSharedPointer<AdbPhoneScreenThread> mPhoneScreenThread;
    void showEvent(QShowEvent*);
    void hideEvent(QHideEvent*);
private slots:
    void on_applicationStateChanged(Qt::ApplicationState);
};

#endif // PHONESCREEN_H
