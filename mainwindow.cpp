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
    centralWidget()->layout()->setContentsMargins(0, 0, 0, 0);
    m_fullScreenWindow = 0;
    if ( encodings().isEmpty() ) {
        encodings() << "Raw" << "Hextile" << "Tight" << "CoRRE" << "Zlib" << "Ultra";
        encodings().sort();
    }
    loadSettings();
    installEventFilter(this);
    QTimer::singleShot(0, this, SLOT(init()));
    rfbClientLog = rfbClientErr = ConnectionWindow::rfbLog;
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::log(QString message)
{
    message.prepend(QTime::currentTime().toString("hh:mm:ss.zzz") + ": ");
    printf("%s\n", message.trimmed().toLocal8Bit().constData());
    fflush(stdout);
}


void MainWindow::init()
{
    if ( QVNCVIEWER_ARG_QUIET )
        ConnectionWindow::setQuiet(true);
    m_recentConnections = globalConfig->mainWindowRecentConnections();
    QTimer::singleShot(0, this, SLOT(initClientFromArguments()));
}

void MainWindow::initClientFromArguments()
{
    QStringList lastArgumentList = {"localhost", "1"};
    if ( lastArgumentList.count() == 2 ) {
        QString hostName = lastArgumentList[0];
        int displayNumber = lastArgumentList[1].toInt();
        ui->connectionWindow->startSessionFromArguments(hostName, displayNumber, QVNCVIEWER_ARG_FULLSCREEN, QVNCVIEWER_ARG_MAXIMIZE);
    }
}

void MainWindow::loadSettings()
{
    restoreGeometry(globalConfig->mainWindowGeometry());
    restoreState(globalConfig->mainWindowState());
}

void MainWindow::saveSettings()
{
    globalConfig->setMainWindowGeometry(saveGeometry());
    globalConfig->setMainWindowState(saveState());
    globalConfig->setMainWindowViewMode(ui->actionWindowViewModeWindowed->isChecked() ? QVNCVIEWER_VIEWMODE_WINDOWED : QVNCVIEWER_VIEWMODE_TABBED);
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
    // QTimer::singleShot(0, qApp, SLOT(quit()));
}
