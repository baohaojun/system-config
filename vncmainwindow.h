#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QCloseEvent>
#include <QStringList>
#include <QString>

#include "ui_vncmainwindow.h"
#include "connectionwindow.h"

#include "luaexecutethread.hpp"
#include <QSharedPointer>
class WrenchMainWindow;

namespace Ui {
class VncMainWindow;
}

class VncMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit VncMainWindow(QWidget *parent = 0);
    ~VncMainWindow();

    ConnectionWindow *fullScreenWindow() { return m_fullScreenWindow; }
    void setFullScreenWindow(ConnectionWindow *w) { m_fullScreenWindow = w; }

    static void log(QString message);
    static QStringList &encodings() { return m_encodings; }

public slots:
    // other
    void init();
    void initClientFromArguments();
    void loadSettings();
    void saveSettings();
    void onVncUpdate(QString);

protected:
    bool eventFilter(QObject *object, QEvent *event);
    void closeEvent(QCloseEvent *e);
    void hideEvent(QHideEvent *e);
    void showEvent(QShowEvent *e);

private:
    Ui::VncMainWindow *ui;
    ConnectionWindow *m_fullScreenWindow;
    QStringList m_recentConnections;
    static QStringList m_encodings;
    WrenchMainWindow* mWrench;
    QSharedPointer<LuaExecuteThread> mLuaThread();
    friend class WrenchMainWindow;
};

#endif // MAINWINDOW_H
