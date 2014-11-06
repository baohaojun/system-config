/**** start of bhj auto includes ****/
/**** end of bhj auto includes ****/

#include "t1wrenchmainwindow.h"
#include "ui_t1wrenchmainwindow.h"
#include <QtCore/QThread>
#include "bhj_help.hpp"
#include "luaexecutethread.hpp"
#include <QtCore/QDebug>
#include <QtCore/QDir>
#include <QtCore/QDate>
#include <QtCore/QDateTime>
#include <QtWidgets/QMessageBox>
#include <QtCore/QProcess>
#include <QtGui/QDesktopServices>
#include <QtCore/QIODevice>
#include <QtCore/QTextStream>
#ifdef Q_OS_WIN32
#include <windows.h>
#include <shellapi.h>
#pragma comment(lib,"shell32.lib")
#endif
#include <QtGui/QPixmap>
#include <QtCore/QCoreApplication>
#include "bhj_help.hpp"
#include <stdlib.h>
#include "lua.hpp"
#include "screencapture.h"
#include <QtWidgets/QFileDialog>

#ifdef Q_OS_WIN32
void setenv(const char* name, const char* val, int overide)
{
    _putenv_s(name, val);
}
#endif

QString emacsWeixinSh;
T1WrenchMainWindow::T1WrenchMainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::T1WrenchMainWindow)
{
    mLuaThread = new LuaExecuteThread(this);
    connect(mLuaThread, SIGNAL(gotSomeLog(QString, QString)), this, SLOT(onInfoUpdate(QString, QString)));
    mLuaThread->start();
    QString pathEnv = QProcessEnvironment::systemEnvironment().value("PATH");
#ifdef Q_OS_WIN32
    pathEnv += ";";
#else
    pathEnv += ":";
#endif
    pathEnv += QCoreApplication::applicationDirPath();

    QByteArray pathBA = pathEnv.toLocal8Bit();
    char *pathBuf = new char[pathBA.size() + 1];
    strcpy(pathBuf, pathBA.data());
    setenv("PATH", pathBuf, 1);
    delete[] pathBuf;

    emacsWeixinSh = QCoreApplication::applicationDirPath() + QDir::separator() + "emacs-weixin.sh";
    ui->setupUi(this);
    connect(ui->phoneTextEdit, SIGNAL(controlEnterPressed()), this, SLOT(on_sendItPushButton_clicked()));
    ui->phoneTextEdit->setFocus(Qt::OtherFocusReason);
    mLastRadioButton = NULL;
}

T1WrenchMainWindow::~T1WrenchMainWindow()
{
    delete ui;
}

void T1WrenchMainWindow::adbStateUpdated(const QString& state)
{
    ui->adbStateLabel->setText(state);
    if (state.toLower() == "online") {
        ui->adbStateIndicator->setPixmap(QPixmap(":/images/green.png"));
    } else {
        ui->adbStateIndicator->setPixmap(QPixmap(":/images/red.png"));
    }
}

// static void appendCmdOutput(QTextEdit *cmdOutputEdit, const QString& output)
// {
//     cmdOutputEdit->moveCursor(QTextCursor::End);
//     cmdOutputEdit->insertPlainText(output);
// }

QString prompt_user(const QString &info, QMessageBox::StandardButtons buttons = (QMessageBox::Ok | QMessageBox::Cancel))
{
    QMessageBox msgBox;
    msgBox.setText(info);
    msgBox.setStandardButtons(buttons);
    msgBox.setDefaultButton(QMessageBox::Save);
    int ret = msgBox.exec();
    if (ret == QMessageBox::Ok)
        return "yes";
    return "";
}

void open_it(const QString &fileOrUrl)
{
#ifdef Q_OS_WIN32
    if (QFileInfo(fileOrUrl).isDir() || QFileInfo(fileOrUrl.split("/").join("\\")).isDir()) {
        qDebug() << "yes it is a dir";
        QProcess::startDetached("explorer", QStringList(fileOrUrl.split("/").join("\\")));
    } else
#endif
        QDesktopServices::openUrl(QUrl(fileOrUrl));
}

static QString logDir;

QString fixPathName(const QString& path)
{
#ifdef Q_OS_WIN32
    QString res = path;
    return res.replace("/", "\\");
#else
    return path;
#endif
}

QString T1WrenchMainWindow::get_text()
{
    QString text = ui->phoneTextEdit->toPlainText();
    while (text.endsWith("\r") || text.endsWith("\n") || text.endsWith("\t") ||
           text.endsWith(" ")) {
        text = text.left(text.length() -1);
    }
    if (text.isEmpty()) {
        prompt_user("输入手机中的文字必须不能为空", QMessageBox::Ok);
    }
    return text;
}

void T1WrenchMainWindow::onInfoUpdate(const QString& key, const QString& val)
{
    static int nTasks;

    if (key == "getclip-android") {
        ui->phoneTextEdit->insertPlainText(val);
    } else if (key == "") {
        // do nothing.
    } else if (key == "start task") {
        ui->adbStateLabel->setText(QString().sprintf("Tasks: %d", ++nTasks));
    } else if (key == "end task") {
        ui->adbStateLabel->setText(QString().sprintf("Tasks: %d", --nTasks));
    } else {
        ui->cmdOutputEdit->moveCursor(QTextCursor::End);
        QString v = val;
        if (val.isEmpty()) {
            v = "OK";
        }
        ui->cmdOutputEdit->insertPlainText(key + ": " + v + "\n");
        qDebug() << "Unknown key: " << key << " with val: " << val;
    }
}

void T1WrenchMainWindow::on_sendItPushButton_clicked()
{
    QString text = get_text();
    if (text.isEmpty()) {
        return;
    }
    mLuaThread->addScript(QStringList() << "t1_post" << text);
    ui->phoneTextEdit->selectAll();
}

void T1WrenchMainWindow::on_configurePushButton_clicked()
{
    // 实在是不想写异步调用的了。
    if (prompt_user("将检查 adb，继续？") == "yes") {
        QDir::home().mkpath(".android");
        QString adbConfig = QDir::homePath() + QDir::separator() + ".android" + QDir::separator() + "adb_usb.ini";
        QFile adbConfigFile(adbConfig);
        adbConfigFile.open(QIODevice::WriteOnly);
        QTextStream out(&adbConfigFile);
        out << "0x29a9\n";
        out.flush();
        adbConfigFile.close();
        getExecutionOutput("adb kill-server");
        QString uname = getExecutionOutput("adb shell uname");
        if (!uname.contains("Linux")) {
            prompt_user("手机状态错误：uname应该是Linux，无法完成配置：\n\n" + uname);
        }
    }

    if (prompt_user("将会安装操作手机剪贴板的SetClip.apk，继续？") == "yes") {
        QString apk = QCoreApplication::applicationDirPath() + QDir::separator() + "SetClip.apk";
        QString res = getExecutionOutput("adb install -r " + apk);
        if (!res.contains("Success")) {
            prompt_user("SetClip.apk安装失败：\n\n" + res);
        } else {
            prompt_user("SetClip.apk安装成功：\n\n" + res);
        }
    }

    if (prompt_user("将获取手机屏幕尺寸，继续？") != "yes") {
        return;
    }
    QString screenPng = QDir::tempPath() + QDir::separator() + "screencap.png";
    QFile(screenPng).remove();
    getExecutionOutput("adb shell screencap /sdcard/screen.png");
    getExecutionOutput("adb pull /sdcard/screen.png " + screenPng);
    if (!QFile(screenPng).exists()) {
        prompt_user("无法获取手机截图及其屏幕尺寸, 无法完成配置");
        return;
    }

    QPixmap screenPixmap(screenPng);
    int w = screenPixmap.width();
    int h = screenPixmap.height();

    prompt_user(QString().sprintf("width is %d, height is %d", w, h));
    if (w != 1080 || h != 1920) {
        prompt_user("你的手机尺寸未进行过适配，默认只支持1920x1080");
    }
}

void T1WrenchMainWindow::on_tbScreenCapture_clicked()
{
    if (mScreenCapture.isNull()) {
        mScreenCapture = QSharedPointer<ScreenCapture>(new ScreenCapture());
        connect(mScreenCapture.data(), SIGNAL(screenCaptured(const QPixmap &)), SLOT(slotHandleCaptureScreen(const QPixmap &)));
    }
}

void T1WrenchMainWindow::slotHandleCaptureScreen(const QPixmap &pix)
{
    QObject::sender()->deleteLater();
    mScreenCapture.clear();
    if (pix.isNull()) return;

    pix.save("screen-shot.png", "PNG");
    mLuaThread->addScript(QStringList() << "t1_picture" << "screen-shot.png");
}

void T1WrenchMainWindow::on_tbPicture_clicked()
{
    QStringList suffixes;
    suffixes << "png" << "jpg" << "gif" << "bmp";

    QStringList fns = QFileDialog::getOpenFileNames(this, tr("选择图片"), QString(), tr("Image Files(*.png *.jpg *.gif *.bmp)"));
    mLuaThread->addScript((QStringList() << "t1_picture") + fns);
}

void T1WrenchMainWindow::on_tbEmoji_clicked()
{
    prompt_user("暂时还没有实现，请期待下个版本");
}
