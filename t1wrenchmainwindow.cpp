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
#include <QtCore/QSharedPointer>
#include "dialoggetemoji.h"

QString emacsWeixinSh;
T1WrenchMainWindow::T1WrenchMainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::T1WrenchMainWindow),
    mSettings("Smartisan", "Wrench", parent)
{
    on_configurePushButton_clicked();

    emacsWeixinSh = QCoreApplication::applicationDirPath() + QDir::separator() + "emacs-weixin.sh";
    ui->setupUi(this);
    connect(ui->phoneTextEdit, SIGNAL(controlEnterPressed()), this, SLOT(on_sendItPushButton_clicked()));
    connect(ui->phoneTextEdit, SIGNAL(emojiShortcutPressed()), this, SLOT(on_tbEmoji_clicked()));
    ui->phoneTextEdit->setFocus(Qt::OtherFocusReason);
    mLastRadioButton = NULL;
}

T1WrenchMainWindow::~T1WrenchMainWindow()
{
    delete ui;
}

void T1WrenchMainWindow::adbStateUpdated(const QString& state)
{
    if (state.toLower() == "online" && ui->adbStateLabel->text().toLower() != "online") {
        if (!mLuaThread.isNull() && mLuaThread->isRunning()) {
            mLuaThread->addScript(QStringList() << "t1_config");
        } else {
            on_configurePushButton_clicked();
        }
    }
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

QString prompt_user(const QString &info, QMessageBox::StandardButtons buttons = QMessageBox::Ok)
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

QString yes_or_no_p(const QString &info)
{
    return prompt_user(info, QMessageBox::Ok | QMessageBox::Cancel);
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
    QString text = ui->phoneTextEdit->getMyText();
    while (text.endsWith("\r") || text.endsWith("\n") || text.endsWith("\t") ||
           text.endsWith(" ")) {
        text = text.left(text.length() -1);
    }
    return text;
}

void T1WrenchMainWindow::onInfoUpdate(const QString& key, const QString& val)
{
    static int nTasks;

    if (key == "getclip-android") {
        ui->phoneTextEdit->insertPlainText(val);
    } else if (key == "exit") {
        prompt_user(QString().sprintf("后台出错，请重新配置小扳手（重连usb线或点一下配置按钮）:\n\n    %s", qPrintable(val)));
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
    if (text.isEmpty() && mPictures.isEmpty()) {
        prompt_user("输入手机中的文字必须不能为空", QMessageBox::Ok);
        return;
    }

    qDebug() << "Sharing text: " << text << " and pictures: " << mPictures;
    if (ui->tbNotes->isChecked()) {
        mLuaThread->addScript(QStringList() << "get_a_note" << text);
        mPictures.insert(0, "last-pic-notes.png");
        text = "#小扳手便笺#";
        if (! ui->tbWeibo->isChecked() && ! ui->tbWeixin->isChecked()) {
            mLuaThread->addScript((QStringList() << "t1_picture") + mPictures);
            mPictures.clear();
            return;
        }
    }

    bool share = 0;
    if (ui->tbWeibo->isChecked()) {
        share = 1;
        if (mPictures.isEmpty()) {
            mLuaThread->addScript(QStringList() << "t1_share_to_weibo" << text);
        } else {
            mLuaThread->addScript((QStringList() << "picture_to_weibo_share") + mPictures);
            mLuaThread->addScript(QStringList() << "t1_post" << text);
        }
    }

    if (ui->tbWeixin->isChecked()) {
        share = 1;
        if (mPictures.isEmpty()) {
            mLuaThread->addScript(QStringList() << "t1_share_to_weixin" << text);
        } else {
            mLuaThread->addScript((QStringList() << "picture_to_weixin_share") + mPictures);
            mLuaThread->addScript(QStringList() << "t1_post" << text);
        }
    }

    if (share && !mPictures.isEmpty()) {
        mPictures.clear();
        ui->tbPicture->setChecked(false);
        ui->tbScreenCapture->setChecked(false);
    }

    if (! share) {
        mLuaThread->addScript(QStringList() << "t1_post" << text);
    }
    ui->phoneTextEdit->selectAll();
}

void T1WrenchMainWindow::on_configurePushButton_clicked()
{
    bool is_starting = false;
    if (mLuaThread.isNull()) {
        is_starting = true;
    }
    if (!mLuaThread.isNull()) {
        if (mLuaThread->isRunning()) {
            if (yes_or_no_p("后台仍在运行，点 确定 按钮重新配置，点 取消 继续使用当前设置？") != "yes") {
                return;
            }
        }
        mLuaThread->quitLua();
        disconnect(mLuaThread.data(), SIGNAL(gotSomeLog(QString, QString)), this, SLOT(onInfoUpdate(QString, QString)));
        if (!mLuaThread->wait(1000)) {
            for (int i = 0; i < 10; i ++) {
                getExecutionOutput("the-true-adb kill-server");
                getExecutionOutput("the-true-adb devices");
                if (mLuaThread->wait(100)) {
                    break;
                }
            }
        }
        mLuaThread.clear();
    }
    mLuaThread = QSharedPointer<LuaExecuteThread>(new LuaExecuteThread(this));
    connect(mLuaThread.data(), SIGNAL(gotSomeLog(QString, QString)), this, SLOT(onInfoUpdate(QString, QString)));
    mLuaThread->start();
    if (! is_starting) {
        mLuaThread->addScript(QStringList() << "t1_config");
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

    if ((ui->tbWeixin->isChecked() || ui->tbWeibo->isChecked()) && ui->tbScreenCapture->isChecked() && !mPictures.isEmpty()) {
        ui->tbScreenCapture->setChecked(false);
        mPictures.removeOne("screen-shot.png");
        return;
    }

    if ((ui->tbWeixin->isChecked() || ui->tbWeibo->isChecked())) {
        QString text = get_text();
        ui->tbScreenCapture->setCheckable(true);
        ui->tbScreenCapture->setChecked(true);
        if (text.isEmpty()) {
            if (yes_or_no_p("不想说点儿什么了？\n\n点 确定 直接分享，点 取消 输入文字再点 扳动 分享") != "yes") {
                return;
            }
        }
        mPictures = QStringList() << "screen-shot.png";
        emit ui->sendItPushButton->clicked();
    } else {
        mLuaThread->addScript(QStringList() << "t1_picture" << "screen-shot.png");
    }
}

void T1WrenchMainWindow::on_tbPicture_clicked()
{
    if ((ui->tbWeixin->isChecked() || ui->tbWeibo->isChecked()) && !mPictures.isEmpty()) {
        ui->tbPicture->setChecked(false);
        mPictures.clear();
        return;
    }
    QStringList suffixes;
    suffixes << "png" << "jpg" << "gif" << "bmp";

    QStringList fns = QFileDialog::getOpenFileNames(this, tr("选择图片"), QString(), tr("Image Files(*.png *.jpg *.gif *.bmp)"));
    if (fns.isEmpty()) {
        return;
    }

    if ((ui->tbWeixin->isChecked() || ui->tbWeibo->isChecked())) {
        QString text = get_text();
        ui->tbPicture->setCheckable(true);
        ui->tbPicture->setChecked(true);
        if (text.isEmpty()) {
            if (yes_or_no_p("不想说点儿什么了？\n\n点 确定 直接分享，点 取消 输入文字再点 扳动 分享") != "yes") {
                return;
            }
        }
        mPictures = fns;
        emit ui->sendItPushButton->clicked();
    } else {
        mLuaThread->addScript((QStringList() << "t1_picture") + fns);
    }
}

void T1WrenchMainWindow::on_tbEmoji_clicked()
{
    if (mEmojiDialog.isNull()) {
        mEmojiDialog = QSharedPointer<DialogGetEmoji>(new DialogGetEmoji(this));
        connect(mEmojiDialog.data(), SIGNAL(emojiSelected(QString, QString)), ui->phoneTextEdit, SLOT(on_emojiSelected(QString, QString)));
    }
    QPoint pos = mSettings.value("emoji-dialog-pos", QVariant(QPoint(0, 0))).toPoint();
    if (pos != QPoint(0, 0)) {
        mEmojiDialog->move(pos);
    }
    mEmojiDialog->exec();
    pos = mEmojiDialog->pos();
    mSettings.setValue("emoji-dialog-pos", QVariant(pos));
}

void T1WrenchMainWindow::on_tbWeibo_clicked()
{
    if (ui->tbWeibo->isChecked() && mSettings.value("firstTimeWeibo", 1).toInt() == 1) {
        mSettings.setValue("firstTimeWeibo", 0);
        prompt_user("您之后输入的文字和选择的照片、截图将被分享到微博");
    }

    if (!ui->tbWeibo->isChecked() && !ui->tbWeixin->isChecked()) {
        ui->tbPicture->setCheckable(false);
        mPictures.clear();
    }

}

void T1WrenchMainWindow::on_tbWeixin_clicked()
{
    if (ui->tbWeixin->isChecked() && mSettings.value("firstTimeWeixin", 1).toInt() == 1) {
        mSettings.setValue("firstTimeWeixin", 0);
        prompt_user("您之后输入的文字和选择的照片、截图将被分享到微信朋友圈");
    }
    if (!ui->tbWeibo->isChecked() && !ui->tbWeixin->isChecked()) {
        ui->tbPicture->setCheckable(false);
        mPictures.clear();
    }
}

void T1WrenchMainWindow::on_tbThumbsUp_clicked()
{
    mLuaThread->addScript(QStringList() << "t1_spread_it");
    mLuaThread->addScript(QStringList() << "t1_follow_me");
}

void T1WrenchMainWindow::on_tbNotes_clicked()
{
    if (ui->tbNotes->isChecked() && mSettings.value("firstTimeNotes", 1).toInt() == 1) {
        mSettings.setValue("firstTimeNotes", 0);
        prompt_user("您之后输入的文字将生成便笺再发送");
    }
}
