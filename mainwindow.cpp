#include <QDesktopServices>
#include <QApplication>
#include <QKeySequence>
#include <QDateTime>
#include <QShortcut>
#include <QWidget>
#include <QAction>
#include <QTimer>
#include <QUrl>
#include <stdio.h>

#include "macros.h"
#include "mainwindow.h"
#include "qvncviewersettings.h"
#include "aboutdialog.h"

extern QtVncViewerSettings *globalConfig;

QStringList MainWindow::m_encodings;

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    m_preferencesDialog = 0;
    m_fullScreenWindow = 0;
    if ( encodings().isEmpty() ) {
        encodings() << "Raw" << "Hextile" << "Tight" << "CoRRE" << "Zlib" << "Ultra";
        encodings().sort();
    }
    loadSettings();
    installEventFilter(this);
    m_preferencesDialog = new PreferencesDialog(this);
    QTimer::singleShot(0, this, SLOT(init()));
    rfbClientLog = rfbClientErr = ConnectionWindow::rfbLog;
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::log(QString message)
{
    if ( !ConnectionWindow::quiet() ) {
        message.prepend(QTime::currentTime().toString("hh:mm:ss.zzz") + ": ");
        printf("%s\n", message.trimmed().toLocal8Bit().constData());
        fflush(stdout);
    }
}

void MainWindow::on_actionConnectionNew_triggered(bool checked)
{
    ConnectionWindow *connectionWindow = new ConnectionWindow(QString(), ui->mdiArea);
    connect(connectionWindow, SIGNAL(windowClosed()), this, SLOT(connectionWindowClosed()));
    connectionWindow->show();
    if ( globalConfig->mainWindowViewMode() == QVNCVIEWER_VIEWMODE_WINDOWED ) {
        if ( globalConfig->preferencesMaximizeWindows() || mdiArea()->activeSubWindow()->isMaximized() )
            connectionWindow->showMaximized();
        else
            connectionWindow->showNormal();
    } else
        connectionWindow->showMaximized();
    QTimer::singleShot(0, this, SLOT(updateWindowActions()));
}

void MainWindow::on_actionConnectionPreferences_triggered(bool checked)
{
    preferencesDialog()->exec();
}

void MainWindow::on_actionConnectionExit_triggered(bool checked)
{
    close();
}

void MainWindow::on_actionWindowNext_triggered(bool checked)
{
    ui->mdiArea->activateNextSubWindow();
}

void MainWindow::on_actionWindowPrevious_triggered(bool checked)
{
    ui->mdiArea->activatePreviousSubWindow();
}

void MainWindow::on_actionWindowTile_triggered(bool checked)
{
    ui->mdiArea->tileSubWindows();
}

void MainWindow::on_actionWindowCascade_triggered(bool checked)
{
    ui->mdiArea->cascadeSubWindows();
}

void MainWindow::on_actionWindowClose_triggered(bool checked)
{
    ui->mdiArea->closeActiveSubWindow();
}

void MainWindow::on_actionWindowCloseAll_triggered(bool checked)
{
    ui->mdiArea->closeAllSubWindows();
}

void MainWindow::on_actionWindowViewModeWindowed_triggered(bool checked)
{
    ui->actionWindowViewModeWindowed->setChecked(true);
    ui->actionWindowViewModeTabbed->setChecked(false);
    globalConfig->setMainWindowViewMode(QVNCVIEWER_VIEWMODE_WINDOWED);
    ui->mdiArea->setViewMode(QVNCVIEWER_VIEWMODE_WINDOWED);
    if ( !ui->mdiArea->subWindowList().isEmpty() ) {
        ui->actionWindowCascade->setEnabled(true);
        ui->actionWindowTile->setEnabled(true);
        if ( globalConfig->preferencesMaximizeWindows() )
            foreach (QMdiSubWindow *w, mdiArea()->subWindowList())
                w->showMaximized();
    }
}

void MainWindow::on_actionWindowViewModeTabbed_triggered(bool checked)
{
    ui->actionWindowViewModeWindowed->setChecked(false);
    ui->actionWindowViewModeTabbed->setChecked(true);
    globalConfig->setMainWindowViewMode(QVNCVIEWER_VIEWMODE_TABBED);
    ui->mdiArea->setViewMode(QVNCVIEWER_VIEWMODE_TABBED);
    ui->actionWindowCascade->setEnabled(false);
    ui->actionWindowTile->setEnabled(false);
}

void MainWindow::on_actionWindowViewModeToggleFullScreen_triggered(bool checked)
{
    if ( fullScreenWindow() )
        fullScreenWindow()->on_toolButtonToggleFullScreen_clicked();
    else {
        ConnectionWindow *connectionWindow = (ConnectionWindow *)ui->mdiArea->activeSubWindow();
        if ( connectionWindow )
            connectionWindow->on_toolButtonToggleFullScreen_clicked();
    }
}

void MainWindow::on_actionHelpAbout_triggered(bool checked)
{
    AboutDialog aboutDialog(this);
    aboutDialog.exec();
}

void MainWindow::on_actionHelpWiki_triggered(bool checked)
{
    QDesktopServices::openUrl(QUrl::fromUserInput("http://wiki.batcom-it.net/index.php?title=VNC_OSD_interface"));
}

void MainWindow::on_actionHelpForum_triggered(bool checked)
{
    QVNCVIEWER_PRINT_TXT(FIXME: MainWindow::on_actionHelpForum_triggered());
}

void MainWindow::on_actionHelpBugTracker_triggered(bool checked)
{
    QDesktopServices::openUrl(QUrl::fromUserInput("http://tracker.batcom-it.net/view_all_bug_page.php?project_id=2"));
}

void MainWindow::on_actionHelpAboutQt_triggered(bool checked)
{
    qApp->aboutQt();
}

void MainWindow::init()
{
    preferencesDialog()->applySettings();
    if ( QVNCVIEWER_ARG_QUIET )
        ConnectionWindow::setQuiet(true);
    m_recentConnections = globalConfig->mainWindowRecentConnections();
    foreach (QString tgt, m_recentConnections)
        ui->menuConnectionRecent->addAction(tgt, this, SLOT(openRecentConnection()));
    QTimer::singleShot(0, this, SLOT(initClientFromArguments()));
}

void MainWindow::initClientFromArguments()
{
    QStringList lastArgumentList = qApp->arguments().last().split(":");
    if ( lastArgumentList.count() == 2 ) {
        QString hostName = lastArgumentList[0];
        int displayNumber = lastArgumentList[1].toInt();
        on_actionConnectionNew_triggered();
        if ( !mdiArea()->subWindowList().isEmpty() ) {
            ConnectionWindow *connectionWindow = (ConnectionWindow *)mdiArea()->subWindowList().first();
            connectionWindow->startSessionFromArguments(hostName, displayNumber, QVNCVIEWER_ARG_FULLSCREEN, QVNCVIEWER_ARG_MAXIMIZE);
        }
    }
}

void MainWindow::connectionWindowClosed()
{
    ConnectionWindow *connectionWindow = (ConnectionWindow *)sender();
    connectionWindow->deleteLater();
    QTimer::singleShot(0, this, SLOT(updateWindowActions()));
}

void MainWindow::updateWindowActions()
{
    bool enable = (!ui->mdiArea->subWindowList().isEmpty() || fullScreenWindow());
    bool single = (ui->mdiArea->subWindowList().count() == 1 || (ui->mdiArea->subWindowList().count() == 0 && fullScreenWindow()));
    bool windowed = (globalConfig->mainWindowViewMode() == QVNCVIEWER_VIEWMODE_WINDOWED);
    ui->actionWindowNext->setEnabled(enable && !single);
    ui->actionWindowPrevious->setEnabled(enable && !single);
    ui->actionWindowCascade->setEnabled(enable && windowed);
    ui->actionWindowTile->setEnabled(enable && windowed);
    ui->actionWindowClose->setEnabled(enable);
    ui->actionWindowCloseAll->setEnabled(enable && !single);
    ui->actionWindowViewModeToggleFullScreen->setEnabled(enable);
}

void MainWindow::loadSettings()
{
    restoreGeometry(globalConfig->mainWindowGeometry());
    restoreState(globalConfig->mainWindowState());
    if ( globalConfig->mainWindowViewMode() == QVNCVIEWER_VIEWMODE_WINDOWED )
        on_actionWindowViewModeWindowed_triggered();
    else
        on_actionWindowViewModeTabbed_triggered();
}

void MainWindow::saveSettings()
{
    globalConfig->setMainWindowGeometry(saveGeometry());
    globalConfig->setMainWindowState(saveState());
    globalConfig->setMainWindowViewMode(ui->actionWindowViewModeWindowed->isChecked() ? QVNCVIEWER_VIEWMODE_WINDOWED : QVNCVIEWER_VIEWMODE_TABBED);
}

void MainWindow::addRecentConnection(const QString &serverAddress, const int &displayNumber)
{
    if ( !serverAddress.isEmpty() and displayNumber >= 0 ) {
        QString vncTarget(serverAddress + ":" + QString::number(displayNumber));
        m_recentConnections.removeAll(vncTarget);
        m_recentConnections.insert(0, vncTarget);
        if ( m_recentConnections.count() > QVNCVIEWER_MAX_RECENT_CONNECTIONS )
            m_recentConnections.removeAt(m_recentConnections.count() - 1);
        ui->menuConnectionRecent->clear();
        foreach (QString tgt, m_recentConnections)
            ui->menuConnectionRecent->addAction(tgt, this, SLOT(openRecentConnection()));
        globalConfig->setMainWindowRecentConnections(m_recentConnections);
    }
}

void MainWindow::openRecentConnection()
{
    QAction *action = (QAction *)sender();
    QStringList tgt = action->text().split(":", QString::SkipEmptyParts);
    if ( tgt.count() == 2 ) {
        QString hostName = tgt[0];
        int displayNumber = tgt[1].toInt();
        on_actionConnectionNew_triggered();
        ConnectionWindow *connectionWindow = (ConnectionWindow *)mdiArea()->subWindowList().last();
        connectionWindow->startSessionFromArguments(hostName, displayNumber, false, false);
    }
}

bool MainWindow::eventFilter(QObject *object, QEvent *event)
{
    if ( event->type() == QEvent::KeyPress ) {
        QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
        if ( QVNCVIEWER_FULLSCREEN_TOGGLED )
            QTimer::singleShot(0, this, SLOT(on_actionWindowViewModeToggleFullScreen_triggered()));
    }
    return QMainWindow::eventFilter(object, event);
}

void MainWindow::closeEvent(QCloseEvent *e)
{
    saveSettings();
    e->accept();
    QTimer::singleShot(0, qApp, SLOT(quit()));
}
