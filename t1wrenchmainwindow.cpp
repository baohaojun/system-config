#include "t1wrenchmainwindow.h"
#include "ui_t1wrenchmainwindow.h"
#include <QtCore/QThread>
#include "bhj_help.hpp"
#include "readinfothread.hpp"
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


const char* qpstDir = "C:\\Program Files (x86)\\Qualcomm\\QPST";
const char* qdartDir = "C:\\Program Files (x86)\\Qualcomm\\QDART";
const char* qcnTool = "C:\\Program Files (x86)\\Qualcomm\\QDART\\bin\\qcnTool.exe";

T1WrenchMainWindow::T1WrenchMainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::T1WrenchMainWindow)
{
    if (QFile("C:\\Program Files\\Qualcomm\\QPST").exists()) {
        qpstDir = "C:\\Program Files\\Qualcomm\\QPST";
        qdartDir = "C:\\Program Files\\Qualcomm\\QDART";
        qcnTool = "C:\\Program Files\\Qualcomm\\QDART\\bin\\qcnTool.exe";
    }
    ui->setupUi(this);
    ui->qqHintLabel->setText("<a href='http://baohaojun.github.io/blog/2014/06/23/0-sending-weixin-weibo-etc-with-emacs-and-smartisa-t1.html'>锤子手机小扳手1.0</a>");
    ui->qqHintLabel->setOpenExternalLinks(true);
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
        ui->adbStateIndicator->setChecked(1);
    } else {
        ui->adbStateIndicator->setChecked(0);
    }
}

static void updateEditTextKeyVal(QTextEdit* edit, const QString& key, const QString& val)
{
    QString text = edit->toPlainText();
    QStringList keyVals = text.split("\n");
    if (keyVals.filter(QRegExp("^" + key)).isEmpty()) {
        keyVals << (key + val);
        keyVals.sort();
    } else {
        keyVals.replaceInStrings(QRegExp("^" + key + ".*"), (key + val));
    }
    while (keyVals.removeOne("")) ; // remove empty ones
    edit->setText(keyVals.join("\n"));
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


void do_progressive_command(T1WrenchMainWindow* window, const QString& cmd, const QStringList& args, const QString& returnKey, bool verifyRet = false)
{
    ReadInfoThread* infoThread = new ReadInfoThread(window);
    window->connect(infoThread, SIGNAL(updateMobileInfo(QString, QString)), window, SLOT(onInfoUpdate(QString, QString)));
    infoThread->setCommandArgsAndReturnKey(cmd, args, returnKey, verifyRet);
    infoThread->setProgressive();
    infoThread->start();
}

static void do_button_click(T1WrenchMainWindow* window, const QList<QStringList>& cmds, const QString& returnKey, bool verifyRet = true)
{
    ReadInfoThread* infoThread = new ReadInfoThread(window);
    window->connect(infoThread, SIGNAL(updateMobileInfo(QString, QString)), window, SLOT(onInfoUpdate(QString, QString)));
    infoThread->setMultipleCommandsAndReturnKey(cmds, returnKey, verifyRet);
    infoThread->start();
}

static void do_button_click(T1WrenchMainWindow* window, const QString& cmd, const QStringList& args, const QString& returnKey, bool verifyRet = true)
{
    ReadInfoThread* infoThread = new ReadInfoThread(window);
    window->connect(infoThread, SIGNAL(updateMobileInfo(QString, QString)), window, SLOT(onInfoUpdate(QString, QString)));
    infoThread->setCommandArgsAndReturnKey(cmd, args, returnKey, verifyRet);
    infoThread->start();
}

static void do_button_click(T1WrenchMainWindow* window, const QString& cmd, const QString& returnKey, bool verifyRet = true)
{
    do_button_click(window, cmd, QStringList(), returnKey, verifyRet);
}

QString quoteCmdArg(const QString& arg)
{
    QString res = arg;
    res.replace("\"", "\"\"\"");
    return "\"" + res + "\"";
}

QString fixPathName(const QString& path)
{
#ifdef Q_OS_WIN32
    QString res = path;
    return res.replace("/", "\\");
#else
    return path;
#endif
}

void T1WrenchMainWindow::putclip_android()
{
    QString text = ui->phoneTextEdit->toPlainText();
    while (text.endsWith("\r") || text.endsWith("\n") || text.endsWith("\t") ||
           text.endsWith(" ")) {
        text = text.left(text.length() -1);
    }
    if (text.isEmpty()) {
        prompt_user("输入手机中的文字必须不能为空", QMessageBox::Ok);
    }

    QDir tmpDir = QDir::temp();
    QFile phoneTxtFile(tmpDir.filePath("send-to-phone.txt"));
    phoneTxtFile.open(QIODevice::WriteOnly);
    QTextStream out(&phoneTxtFile);
    out << text;
    out.flush();
    phoneTxtFile.close();

    QList<QStringList> cmds;
    cmds << (QStringList() << "adb" << "push" << tmpDir.filePath("send-to-phone.txt") << "/sdcard/putclip.txt")
         << (QStringList() << "adb" << "shell" << "am startservice -n com.bhj.setclip/.PutClipService;"
             "for x in $(seq 1 20); do if test -e /sdcard/putclip.txt; then busybox sleep .1; echo $x; else exit; fi; done;");

    do_button_click(this, cmds, "putclip-android", false);
}

void T1WrenchMainWindow::on_qqButton_clicked()
{

}

void T1WrenchMainWindow::on_cellMailButton_clicked()
{
    putclip_android();

    QList<QStringList> cmds2;
    cmds2 << (QStringList() << "adb shell input touchscreen swipe 586 878 586 268 500")
          << (QStringList() << "adb shell input tap 560 1840")
          << (QStringList() << "adb shell input tap 299 299")
          << (QStringList() << "adb shell input tap 299 299")
          << (QStringList() << "adb shell input tap 505 192")
          << (QStringList() << "adb shell input tap 998 174");

    do_button_click(this, cmds2, "click mail", false);
}

void fillFromAdbTaps(QList<QStringList>& cmds, QString& shellScript)
{
    foreach (const QString &sh, shellScript.split("\n")) {
        QString shCopy = sh;
        shCopy.replace(QRegExp("^\\s+"), "");
        shCopy.replace(QRegExp("#.*"), "");
        shCopy.replace(QRegExp("\\s+$"), "");
        if (!shCopy.isEmpty()) {
            if (shCopy.contains(QRegExp("^adb-tap "))) {
                shCopy.replace(QRegExp("^adb-tap "), "adb shell input tap ");
                cmds << QStringList(shCopy);
            } else if (shCopy.contains(QRegExp("^adb-tap-2 "))) {
                shCopy.replace(QRegExp("^adb-tap-2 "), "adb shell input tap ");
                cmds << QStringList(shCopy);
                cmds << QStringList(shCopy);
            } else if (shCopy.contains(QRegExp("^adb-long-press "))) {
                shCopy.replace(QRegExp("^adb-long-press "), "");
                shCopy = "adb shell input touchscreen swipe " + shCopy + " " + shCopy + " 550";
                cmds << QStringList(shCopy);
            } else if (shCopy == "adb-key") {
                cmds << QStringList(shCopy);
            } else {
                prompt_user("Unknown shell script: " + shCopy );
            }
        }
    }
}

void T1WrenchMainWindow::on_t1SmsButton_clicked()
{
    putclip_android();

    QString t1ShellScript =
        "                    adb-tap 560 1840 # 点空格\n"
        "                    adb-long-press 522 912 # 长按输入框\n"
        "                    adb-tap 480 802\n"
        "                    adb-tap 864 921\n";

    QList<QStringList> cmds2;
    fillFromAdbTaps(cmds2, t1ShellScript);
    do_button_click(this, cmds2, "click mail", false);
}

void T1WrenchMainWindow::on_weiboButton_clicked()
{
    putclip_android();

    QString t1ShellScript =
        "                    adb shell input keyevent SPACE\n"
        "                    adb-long-press 440 281\n"
        "                    adb-tap 545 191\n"
        "                    adb-tap 991 166\n";

    QList<QStringList> cmds2;
    fillFromAdbTaps(cmds2, t1ShellScript);
    do_button_click(this, cmds2, "click weibo", false);
}

void T1WrenchMainWindow::on_googlePlusButton_clicked()
{
    putclip_android();

    QString t1ShellScript =
        "                    adb-tap 560 1840\n"
        "                    adb-long-press 99 383 # long press\n"
        "                    adb-tap 497 281 # paste\n"
        "                    adb-tap 985 935 # send\n";

    QList<QStringList> cmds2;
    fillFromAdbTaps(cmds2, t1ShellScript);
    do_button_click(this, cmds2, "click google+", false);

}

void T1WrenchMainWindow::on_toPhoneClipboardButton_clicked()
{
    putclip_android();
}

void T1WrenchMainWindow::getclip_android()
{
    QStringList args;
    args << "shell"
         << "rm -f /sdcard/putclip.txt;"
        "am startservice -n com.bhj.setclip/.PutClipService --ei getclip 1 >/dev/null 2>&1&\n"
        "for x in $(seq 1 20); do if test -e /sdcard/putclip.txt; then cat /sdcard/putclip.txt; break; else sleep .1; fi; done";

    do_button_click(this, "adb", args, "getclip-android", false);
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

void T1WrenchMainWindow::on_weixinQqRadio_toggled(bool checked)
{
    if (checked == false) {
        mLastRadioButton = ui->weixinQqRadio;
    } else {
        ui->qqHintLabel->setPixmap(QPixmap(":/images/weixin.png").scaled(ui->qqHintLabel->width(), ui->qqHintLabel->height()));
    }
}

void T1WrenchMainWindow::on_replyMailRadio_toggled(bool checked)
{
    if (checked == false) {
        mLastRadioButton = ui->replyMailRadio;
    } else {
        ui->qqHintLabel->setPixmap(QPixmap(":/images/cell-mail.png").scaled(ui->qqHintLabel->width(), ui->qqHintLabel->height()));
    }
}

void T1WrenchMainWindow::on_replySmsRadio_toggled(bool checked)
{
    if (checked == false) {
        mLastRadioButton = ui->replySmsRadio;
    } else {
        ui->qqHintLabel->setText("暂无适用场景示意图");
    }
}

void T1WrenchMainWindow::on_weiboRadio_toggled(bool checked)
{
    if (checked == false) {
        mLastRadioButton = ui->weiboRadio;
    } else {
        ui->qqHintLabel->setText("暂无适用场景示意图");
    }
}

void T1WrenchMainWindow::on_googlePlusRadio_toggled(bool checked)
{
    if (checked == false) {
        mLastRadioButton = ui->googlePlusRadio;
    } else {
        ui->qqHintLabel->setText("暂无适用场景示意图");
    }
}

void T1WrenchMainWindow::on_toClipBoardRadio_toggled(bool checked)
{
    if (checked == false) {
        mLastRadioButton = ui->toClipBoardRadio;
    } else {
        ui->qqHintLabel->setText("把左边编辑框里的内容\n放到手机的剪贴板里去");
    }
}

void T1WrenchMainWindow::on_fromClipBoard_toggled(bool checked)
{
    if (checked == false) {
        mLastRadioButton = ui->fromClipBoard;
    } else {
        ui->qqHintLabel->setText("把手机剪贴板里的内容\n粘到左边的编辑框中");
    }
}

QString getActionScript(const QString& scenario)
{
    QString emacsWeixinSh = QCoreApplication::applicationDirPath() + QDir::separator() + "emacs-weixin.sh";
    QFile emacsWeixinFile(emacsWeixinSh);
    if (!emacsWeixinFile.open(QIODevice::ReadOnly | QIODevice::Text))
        return "";

    QTextStream in(&emacsWeixinFile);
    QString res;
    while (!in.atEnd()) {
        QString line = in.readLine();
        if (line.contains(scenario)) {
            while (!in.atEnd()) {
                QString add = in.readLine();
                if (add.contains(QRegExp("^\\s*;;\\s*$"))) {
                    goto end;
                }
                res += add;
            }
        }
    }
end:
    return res;
}

void T1WrenchMainWindow::on_sendItPushButton_clicked()
{
    if (ui->fromClipBoard->isChecked()) {
        getclip_android();
        return;
    }
    putclip_android();
    if (ui->toClipBoardRadio->isChecked()) {
        return;
    }


    QString actionShellScript;
    if (ui->weixinQqRadio->isChecked()) {
        actionShellScript = getActionScript("最常见情形");
    } else if (ui->replyMailRadio->isChecked()) {
        actionShellScript = getActionScript("回邮件");
    } else if (ui->replySmsRadio->isChecked()) {
        actionShellScript = getActionScript("快速回短信");
    } else if (ui->weiboRadio->isChecked()) {
        actionShellScript = getActionScript("发微博");
    } else if (ui->googlePlusRadio->isChecked()) {
        actionShellScript = getActionScript("发 Google Plus");
    }

    qDebug() << "actionShellScript is '" << actionShellScript <<"'";
    QList<QStringList> cmds;
    fillFromAdbTaps(cmds, actionShellScript);
    do_button_click(this, cmds, "action clicked", false);
}
