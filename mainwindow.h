#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QCloseEvent>
#include <QStringList>
#include <QString>

#include "ui_mainwindow.h"
#include "connectionwindow.h"

#include "luaexecutethread.hpp"
#include <QSharedPointer>
class WrenchMainWindow;

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

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

protected:
    bool eventFilter(QObject *object, QEvent *event);
    void closeEvent(QCloseEvent *e);

private:
    Ui::MainWindow *ui;
    ConnectionWindow *m_fullScreenWindow;
    QStringList m_recentConnections;
    static QStringList m_encodings;
    WrenchMainWindow* mWrench;
    QSharedPointer<LuaExecuteThread> mLuaThread();
    friend class WrenchMainWindow;
};

#endif // MAINWINDOW_H
