/* -*- mode: c++ -*- */
#ifndef PHONESCREEN_H
#define PHONESCREEN_H

#include <QDialog>
class PhoneScreenDialog;
class AdbPhoneScreenThread;
#include "adbphonescreenthread.hpp"
#include <QSharedPointer>
#include <QWindow>
#include "luaexecutethread.hpp"
#include <QSharedPointer>
class T1WrenchMainWindow;

namespace Ui {
class PhoneScreenDialog;
}

class PhoneScreenDialog : public QDialog
{
    Q_OBJECT

public:
    explicit PhoneScreenDialog(QWidget *parent = 0);
    ~PhoneScreenDialog();

public slots:
    void phoneScreenUpdated();
    void syncScreen();

protected:
    bool eventFilter(QObject *obj, QEvent *ev);
    void resizeEvent(QResizeEvent *);

private:
    bool isShelled;
    void reloadBackground();
    void closeEvent(QCloseEvent *event);
    Ui::PhoneScreenDialog *ui;
    AdbPhoneScreenThread* mPhoneScreenThread;
    void showEvent(QShowEvent*);
    void hideEvent(QHideEvent*);
    QSharedPointer<LuaExecuteThread> mLuaThread();
    T1WrenchMainWindow* mT1Wrench;
    friend class T1WrenchMainWindow;
};

#endif // PHONESCREEN_H
