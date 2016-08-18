#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QCloseEvent>
#include <QStringList>
#include <QString>

#include "ui_mainwindow.h"
#include "connectionwindow.h"
#include "preferencesdialog.h"

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

    QMdiArea *mdiArea() { return ui->mdiArea; }
    PreferencesDialog *preferencesDialog() { return m_preferencesDialog; }
    ConnectionWindow *fullScreenWindow() { return m_fullScreenWindow; }
    void setFullScreenWindow(ConnectionWindow *w) { m_fullScreenWindow = w; }

    static void log(QString message);
    static QStringList &encodings() { return m_encodings; }

public slots:
    // callbacks
    void on_actionConnectionNew_triggered(bool checked = false);
    void on_actionConnectionPreferences_triggered(bool checked = false);
    void on_actionConnectionExit_triggered(bool checked = false);

    void on_actionWindowNext_triggered(bool checked = false);
    void on_actionWindowPrevious_triggered(bool checked = false);
    void on_actionWindowTile_triggered(bool checked = false);
    void on_actionWindowCascade_triggered(bool checked = false);
    void on_actionWindowClose_triggered(bool checked = false);
    void on_actionWindowCloseAll_triggered(bool checked = false);
    void on_actionWindowViewModeWindowed_triggered(bool checked = false);
    void on_actionWindowViewModeTabbed_triggered(bool checked = false);
    void on_actionWindowViewModeToggleFullScreen_triggered(bool checked = false);

    void on_actionHelpAbout_triggered(bool checked = false);
    void on_actionHelpWiki_triggered(bool checked = false);
    void on_actionHelpForum_triggered(bool checked = false);
    void on_actionHelpBugTracker_triggered(bool checked = false);
    void on_actionHelpAboutQt_triggered(bool checked = false);

    // other
    void init();
    void initClientFromArguments();
    void connectionWindowClosed();
    void updateWindowActions();
    void loadSettings();
    void saveSettings();

protected:
    bool eventFilter(QObject *object, QEvent *event);
    void closeEvent(QCloseEvent *e);

private:
    Ui::MainWindow *ui;
    PreferencesDialog *m_preferencesDialog;
    ConnectionWindow *m_fullScreenWindow;
    QStringList m_recentConnections;
    static QStringList m_encodings;
};

#endif // MAINWINDOW_H
