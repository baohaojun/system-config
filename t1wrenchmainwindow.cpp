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
#include <QInputDialog>
#include <QMenu>
#include <QSystemTrayIcon>
#include <QAction>
#include <QIcon>
#include <QMimeData>
#include <QUrl>
#include <QList>
#include <QFileInfo>
#include "emojimodel.h"
#include "contactmodel.h"
#include "strlistmodel.h"
#include "phonescreen.h"

QString emacsWeixinSh;
T1WrenchMainWindow::T1WrenchMainWindow(QWidget *parent) :
    QMainWindow(parent),
    mQuit(false),
    ui(new Ui::T1WrenchMainWindow),
    mSettings("Smartisan", "Wrench", parent)
{
    on_configurePushButton_clicked();

    emacsWeixinSh = QCoreApplication::applicationDirPath() + QDir::separator() + "emacs-weixin.sh";
    ui->setupUi(this);
    connect(ui->phoneTextEdit, SIGNAL(controlEnterPressed()), this, SLOT(on_sendItPushButton_clicked()));
    connect(ui->phoneTextEdit, SIGNAL(emojiShortcutPressed()), this, SLOT(on_tbEmoji_clicked()));
    connect(ui->phoneTextEdit, SIGNAL(phoneCallShortcutPressed()), this, SLOT(on_tbPhoneCall_clicked()));
    connect(ui->phoneTextEdit, SIGNAL(MmsShortcutPressed()), this, SLOT(on_tbMms_clicked()));
    ui->phoneTextEdit->setFocus(Qt::OtherFocusReason);
    mLastRadioButton = NULL;
    createTrayIcon();
    this->setAcceptDrops(true);
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

void T1WrenchMainWindow::onSelectArgs(const QStringList& args)
{
    QString prompt = args[0];
    QStringList argsCopy = args;
    argsCopy.pop_front();
    StrlistModel model(argsCopy);

    DialogGetEntry dialog(&model, prompt, this);
    connect(&dialog, SIGNAL(entrySelected(QString)), this, SLOT(on_argSelected(QString)));
    mSelectArgDialog = &dialog;
    dialog.exec();
    if (mSelectArgDialog != NULL) {
        on_argSelected(args[1]);
    } else {
        qDebug() << qPrintable(QString().sprintf("%s:%d:", __FILE__, __LINE__));
    }

    dialog.disconnect();
}

void T1WrenchMainWindow::on_sendItPushButton_clicked()
{
    QString text = get_text();
    if (text.isEmpty() && mPictures.isEmpty()) {
        prompt_user("输入手机中的文字必须不能为空", QMessageBox::Ok);
        return;
    }

    if (ui->tbNotes->isChecked()) {
        mLuaThread->addScript(QStringList() << "get_a_note" << text);
        mPictures.insert(0, "last-pic-notes.png");
        text = "#小扳手便笺#";
        if (! ui->tbWeibo->isChecked() && ! ui->tbWeixin->isChecked() && ! ui->tbMomo->isChecked()) {
            mLuaThread->addScript((QStringList() << "t1_picture") + mPictures);
            mPictures.clear();
            return;
        } else {
            bool ok = false;
            text = QInputDialog::getText(this, tr("QInputDialog::getText()"),
                                         tr("请输入分享文字内容"), QLineEdit::Normal,
                                         text, &ok);
            if (!ok) {
                text = "#小扳手便笺#";
            }
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
        ui->tbWeibo->setChecked(false);
    }

    if (ui->tbWeixin->isChecked()) {
        share = 1;
        if (mPictures.isEmpty()) {
            mLuaThread->addScript(QStringList() << "t1_share_to_weixin" << text);
        } else {
            mLuaThread->addScript((QStringList() << "picture_to_weixin_share") + mPictures);
            mLuaThread->addScript(QStringList() << "t1_post" << text);
        }
        ui->tbWeixin->setChecked(false);
    }

    if (ui->tbMomo->isChecked()) {
        share = 1;
        if (mPictures.isEmpty()) {
            prompt_user("陌陌分享失败，没有图片不能分享（可以考虑用便签）。");
        } else {
            mLuaThread->addScript((QStringList() << "picture_to_momo_share") + mPictures);
            mLuaThread->addScript(QStringList() << "t1_post" << text);
        }
        ui->tbMomo->setChecked(false);
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
        mLuaThread->disconnect();

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
    connect(mLuaThread.data(), SIGNAL(selectArgsSig(QStringList)), this, SLOT(onSelectArgs(QStringList)));
    connect(mLuaThread.data(), SIGNAL(load_mail_heads_sig(QString, QString, QString, QString, QString)), this, SLOT(onLoadMailHeads(QString, QString, QString, QString, QString)));
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

    if ((ui->tbWeixin->isChecked() || ui->tbWeibo->isChecked() || ui->tbMomo->isChecked()) &&
        ui->tbScreenCapture->isChecked() &&
        !mPictures.isEmpty()) {
        ui->tbScreenCapture->setChecked(false);
        mPictures.removeOne("screen-shot.png");
        return;
    }

    if ((ui->tbWeixin->isChecked() || ui->tbWeibo->isChecked() || ui->tbMomo->isChecked())) {
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
    if ((ui->tbWeixin->isChecked() || ui->tbWeibo->isChecked() || ui->tbMomo->isChecked()) && !mPictures.isEmpty()) {
        ui->tbPicture->setChecked(false);
        mPictures.clear();
        return;
    }
    QStringList suffixes;
    suffixes << "png" << "jpg" << "gif" << "bmp";

    QStringList fns = QFileDialog::getOpenFileNames(this, tr("选择图片/视频"), QString(), tr("Image/Video Files(*.png *.jpg *.gif *.bmp *.mp4 *.avi)"));
    if (fns.isEmpty()) {
        return;
    }

    if ((ui->tbWeixin->isChecked() || ui->tbWeibo->isChecked() || ui->tbMomo->isChecked())) {
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
        mEmojiDialog = QSharedPointer<DialogGetEntry>(new DialogGetEntry(new EmojiModel(0), "表情过滤", this));
        connect(mEmojiDialog.data(), SIGNAL(entrySelected(QString)), ui->phoneTextEdit, SLOT(on_emojiSelected(QString)));
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

    if (!ui->tbWeibo->isChecked() && !ui->tbWeixin->isChecked() && !ui->tbMomo->isChecked()) {
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

void T1WrenchMainWindow::on_tbMomo_clicked()
{
    if (ui->tbMomo->isChecked() && mSettings.value("firstTimeMomo", 1).toInt() == 1) {
        mSettings.setValue("firstTimeMomo", 0);
        prompt_user("您之后输入的文字和选择的照片、截图将被分享到陌陌留言板");
    }
    if (!ui->tbWeibo->isChecked() && !ui->tbMomo->isChecked()) {
        ui->tbPicture->setCheckable(false);
        mPictures.clear();
    }
}

void T1WrenchMainWindow::on_tbThumbsUp_clicked()
{
    mLuaThread->addScript(QStringList() << "t1_spread_it");
    mLuaThread->addScript(QStringList() << "t1_follow_me");
}

void T1WrenchMainWindow::initContactDialog(bool isMail)
{
    QString placeHolder = "联系人过滤";
    if (mContactDialog.isNull()) {
        mContactModel = new ContactModel(0);
        mContactDialog = QSharedPointer<DialogGetEntry>(new DialogGetEntry(mContactModel, placeHolder, this));
    }

    if (ui->tbWeixin->isChecked() && !isMail) {
        placeHolder = "微信联系人过滤";
        mContactModel->setWeixin(true);
    } else {
        mContactModel->setWeixin(false);
    }

    mContactDialog->setHint(placeHolder);
    mContactModel->setMail(isMail);
}

void T1WrenchMainWindow::on_tbMms_clicked()
{
    if (ui->tbWeixin->isChecked()) {
        ui->tbWeixin->setChecked(false);
    }
    initContactDialog();
    mMmsReceiverMap.clear();
    connect(mContactDialog.data(), SIGNAL(entrySelectedWithDisplayText(QString, QString)), this, SLOT(on_addMmsReceiver(QString, QString)));

    afterUsingContactDialog();
    if (mMmsReceiverMap.isEmpty()) {
        onInfoUpdate("info", "没有指定短信接收人");
        return;
    }

    QString receivers;
    foreach (const QString& receiver, mMmsReceiverMap.keys()) {
        receivers += receiver + ",";
    }
    mLuaThread->addScript(QStringList() << "t1_add_mms_receiver" << receivers);
}

void T1WrenchMainWindow::afterUsingContactDialog()
{
    QPoint pos = mSettings.value("contact-dialog-pos", QVariant(QPoint(0, 0))).toPoint();
    if (pos != QPoint(0, 0)) {
        mContactDialog->move(pos);
    }
    mContactDialog->exec();
    mContactDialog->disconnect();
    pos = mContactDialog->pos();
    mSettings.setValue("contact-dialog-pos", QVariant(pos));
}
void T1WrenchMainWindow::on_tbPhoneCall_clicked()
{

    initContactDialog();
    connect(mContactDialog.data(), SIGNAL(entrySelected(QString)), this, SLOT(on_Dial(QString)));
    afterUsingContactDialog();
    ui->tbWeixin->setChecked(false);
}

void T1WrenchMainWindow::on_tbNotes_clicked()
{
    if (ui->tbNotes->isChecked() && mSettings.value("firstTimeNotes", 1).toInt() == 1) {
        mSettings.setValue("firstTimeNotes", 0);
        prompt_user("您之后输入的文字将生成便笺再发送");
    }
}

void T1WrenchMainWindow::on_addMmsReceiver(const QString&contact, const QString& display)
{
    if (mMmsReceiverMap.contains(contact) && yes_or_no_p(mMmsReceiverMap[contact] +
                                                         "已经在短信接收人名单里，删除？") == "yes") {
        mMmsReceiverMap.remove(contact);
        return;
    }
    onInfoUpdate("info", "发短信给 " + display);
    mMmsReceiverMap[contact] = display;
}

void T1WrenchMainWindow::on_Dial(const QString&contact)
{
    if (ui->tbWeixin->isChecked()) {
        mLuaThread->addScript(QStringList() << "t1_find_weixin_contact" << contact);
        return;
    }
    mLuaThread->addScript(QStringList() << "t1_call" << contact);
}

void T1WrenchMainWindow::quitMyself()
{
    mQuit = true;
    close();
}

void T1WrenchMainWindow::createTrayIcon()
{

    quitAction = new QAction(tr("&Quit"), this);
    connect(quitAction, SIGNAL(triggered()), this, SLOT(quitMyself()));

    trayIconMenu = new QMenu(this);
    trayIconMenu->addAction(quitAction);

    trayIcon = new QSystemTrayIcon(this);
    trayIcon->setContextMenu(trayIconMenu);

    QIcon icon("emojis/iphone-emoji/WRENCH.png");
    trayIcon->setIcon(icon);

    connect(trayIcon, SIGNAL(activated(QSystemTrayIcon::ActivationReason)),
            this, SLOT(iconActivated(QSystemTrayIcon::ActivationReason)));
    trayIcon->show();
}

void T1WrenchMainWindow::iconActivated(QSystemTrayIcon::ActivationReason reason)
{
    switch (reason) {
    case QSystemTrayIcon::Trigger:
    case QSystemTrayIcon::DoubleClick:
        this->showNormal();
        emit activateWindow();
        break;
    default:
        ;
    }
}

void T1WrenchMainWindow::closeEvent(QCloseEvent *event)
{
    if (mQuit == true) {
        QMainWindow::closeEvent(event);
        return;
    }

    if (trayIcon->isVisible()) {
        hide();
        event->ignore();
    }
}

void T1WrenchMainWindow::startTask(const QString& task)
{
    qDebug() << qPrintable(QString().sprintf("%s:%d:", __FILE__, __LINE__));
    showNormal();
    emit activateWindow();

    QString path = task;
    QFileInfo fileInfo = QFileInfo(path);
    if (fileInfo.isReadable() && fileInfo.isFile()) {
        mLuaThread->addScript(QStringList() << "t1_run" << path);
    }

}

void T1WrenchMainWindow::dragEnterEvent(QDragEnterEvent *event)
{
    event->acceptProposedAction();
}

void T1WrenchMainWindow::dropEvent(QDropEvent *event)
{
    event->acceptProposedAction();
    if (event->mimeData()->hasUrls()) {
        QList<QUrl> urls = event->mimeData()->urls();
        if (urls.size() == 1 && urls[0].isLocalFile()) {
            startTask(urls[0].toLocalFile());
        }
    }
}

void T1WrenchMainWindow::on_argSelected(const QString& arg)
{
    mSelectArgDialog->close();
    mSelectArgDialog = NULL;
    mLuaThread->on_argSelected(arg);
}

void T1WrenchMainWindow::on_tbMailAddTo_clicked()
{
    initContactDialog(true);
    connect(mContactDialog.data(), SIGNAL(entrySelected(QString)), this, SLOT(on_MailTo(QString)));
    afterUsingContactDialog();
}

static void addMailContact(const QString& contact, QPlainTextEdit* where)
{
    QString text = where->toPlainText();
    if (text.startsWith(contact + ",") || text.contains("," + contact + ",")) {
        return;
    }
    where->setPlainText(text + contact + ",");
}

void T1WrenchMainWindow::on_MailTo(const QString& to)
{
    addMailContact(to, ui->ptMailTo);
}

void T1WrenchMainWindow::on_tbMailAddCc_clicked()
{
    initContactDialog(true);
    connect(mContactDialog.data(), SIGNAL(entrySelected(QString)), this, SLOT(on_MailCc(QString)));
    afterUsingContactDialog();
}

void T1WrenchMainWindow::on_MailCc(const QString& to)
{
    addMailContact(to, ui->ptMailCc);
}

void T1WrenchMainWindow::on_tbMailAddBcc_clicked()
{
    initContactDialog(true);
    connect(mContactDialog.data(), SIGNAL(entrySelected(QString)), this, SLOT(on_MailBcc(QString)));
    afterUsingContactDialog();
}

void T1WrenchMainWindow::on_MailBcc(const QString& to)
{
    addMailContact(to, ui->ptMailBcc);
}

void T1WrenchMainWindow::on_tbMailAddAttachment_clicked()
{
    QStringList fns = QFileDialog::getOpenFileNames(this, tr("选择附件"), QString(), tr("All Files(*)"));
    if (fns.isEmpty()) {
        return;
    }
    foreach (const QString& file, fns) {
        ui->ptMailAttachments->setPlainText(ui->ptMailAttachments->toPlainText() + file + "\n");
    }
}

static QStringList getMailHeads(Ui::T1WrenchMainWindow* ui)
{
    return QStringList() << ui->ptMailSubject->toPlainText()
                         << ui->ptMailTo->toPlainText()
                         << ui->ptMailCc->toPlainText()
                         << ui->ptMailBcc->toPlainText()
                         << ui->ptMailAttachments->toPlainText();
}

void T1WrenchMainWindow::on_tbMailDone_clicked()
{
    ui->tabWidget->setCurrentIndex(0);
    ui->phoneTextEdit->setFocus(Qt::OtherFocusReason);
    mLuaThread->addScript((QStringList() << "t1_adb_mail") + getMailHeads(ui));
}

void T1WrenchMainWindow::on_tbMailLoad_clicked()
{
    QString file = QFileDialog::getOpenFileName(0, "请选择打开哪个邮件小扳手脚本文件", QString(), "小扳手脚本文件(*.twa)");
    mLuaThread->addScript(QStringList() << "t1_run" << file);
}

void T1WrenchMainWindow::on_tbMailSave_clicked()
{
    QString file = QFileDialog::getSaveFileName(0, "请选择保存常用联系方式到哪个文件", QString(), "小扳手脚本文件(*.twa)");
    mLuaThread->addScript((QStringList() << "t1_save_mail_heads" << file) + getMailHeads(ui));
}

void T1WrenchMainWindow::on_tbMailClear_clicked()
{
    onLoadMailHeads("", "", "", "", "");
}

void T1WrenchMainWindow::onLoadMailHeads(const QString& subject, const QString& to, const QString& cc, const QString& bcc, const QString& attachments)
{
    ui->ptMailSubject->setPlainText(subject);
    ui->ptMailTo->setPlainText(to);
    ui->ptMailCc->setPlainText(cc);
    ui->ptMailBcc->setPlainText(bcc);
    ui->ptMailAttachments->setPlainText(attachments);
    ui->tabWidget->setCurrentIndex(1);
}

void T1WrenchMainWindow::on_tbPhoneScreen_toggled(bool checked)
{
    if (checked) {
        if (mPhoneScreenDialog.isNull()) {
            mPhoneScreenDialog = QSharedPointer<PhoneScreen>(new PhoneScreen(this));
            mPhoneScreenDialog->setModal(false);
            mPhoneScreenDialog->show();
            int winId = mPhoneScreenDialog->winId();
            system(QString().sprintf("set -x; the-true-adb shell screenrecord --raw - |mplayer --wid=%d -&", winId).toUtf8().constData());
        }
    } else {

    }
}
