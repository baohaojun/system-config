/**** start of bhj auto includes ****/
/**** end of bhj auto includes ****/

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
//#include "screencapture.h"
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
#include "appsmodel.h"
#include "phonescreendialog.h"
#include <QStandardPaths>
#include "wrench.h"
#include "vncmainwindow.h"
#include "adbvncthread.hpp"
#include <QtCore/QRect>
#include <QProcessEnvironment>
#include "wrenchmainwindow.h"
#include "ui_wrenchmainwindow.h"
#include "notificationmodel.h"
#include <QtGui/QKeySequence>
#include <QtWidgets/QToolButton>
#include <QtCore/QRegularExpression>
#include <QtGui/QImage>
#include <QNetworkRequest>
#include <QNetworkReply>
#include "adbclient.h"

static const int max_notification_entries = 1000;

WrenchMainWindow::WrenchMainWindow(QWidget *parent) :
    QMainWindow(parent),
    mQuit(false),
    ui(new Ui::WrenchMainWindow),
    mSettings("Smartisan", "Wrench", parent),
    m_hotkey(QKeySequence("Ctrl+F6"), true),
    m_defaultIcon("emojis/iphone-emoji/WRENCH.png"),
    mNotificationOnline(false),
    mInputOnline(false)
{
    on_configurePushButton_clicked();
    weixinSelectContactTimer.setSingleShot(true);
    qqSelectContactTimer.setSingleShot(true);
    weiboSelectContactTimer.setSingleShot(true);

    connect(&weiboSelectContactTimer, SIGNAL(timeout()), this, SLOT(selectWeiboContact()));
    connect(&weixinSelectContactTimer, SIGNAL(timeout()), this, SLOT(selectWeixinContact()));
    connect(&qqSelectContactTimer, SIGNAL(timeout()), this, SLOT(selectQqContact()));

    ui->setupUi(this);
    connect(&m_manager, SIGNAL(finished(QNetworkReply*)),
            this, SLOT(handleNetworkData(QNetworkReply*)));

    connect(ui->phoneTextEdit, SIGNAL(controlEnterPressed()), this, SLOT(on_sendItPushButton_clicked()));
    connect(ui->phoneTextEdit, SIGNAL(emojiShortcutPressed()), this, SLOT(on_tbEmoji_clicked()));
    connect(ui->phoneTextEdit, SIGNAL(phoneCallShortcutPressed()), this, SLOT(on_tbPhoneCall_clicked()));
    connect(ui->phoneTextEdit, SIGNAL(imageDropEvent(QDropEvent)), this, SLOT(imageDropped(QDropEvent)));
    ui->phoneTextEdit->setFocus(Qt::OtherFocusReason);
    mLastRadioButton = NULL;
    createTrayIcon();
    this->setAcceptDrops(true);
    ui->phoneTextEdit->installEventFilter(this);
    ui->ptMailSubject->installEventFilter(this);
    ui->ptMailTo->installEventFilter(this);
    ui->ptMailCc->installEventFilter(this);
    ui->ptMailBcc->installEventFilter(this);
    ui->ptMailAttachments->installEventFilter(this);
    QList<QToolButton*> tbs = findChildren<QToolButton*>(QRegularExpression(".*"));
    foreach(QToolButton* tb, tbs) {
        tb->installEventFilter(this);
    }

    qDebug() << "Is Registered: " << m_hotkey.isRegistered();
    connect(&m_hotkey, SIGNAL(activated()), this, SLOT(slotShortCutActivated()));

    m_snore = &Snore::SnoreCore::instance();
    m_snore->loadPlugins( Snore::SnorePlugin::Backend);

    m_snore_application = Snore::Application("Wrench", m_defaultIcon);
    m_alert = Snore::Alert("Wrench", m_defaultIcon);
#ifdef Q_OS_DARWIN
    m_snore->setSettingsValue(QStringLiteral("PrimaryBackend"), QProcessEnvironment::systemEnvironment().value("SNORE_BACKEND", QStringLiteral("Snore")), Snore::LocalSetting);
#endif
    m_snore_application.addAlert(m_alert);

    m_snore->registerApplication(m_snore_application);
    connect( m_snore, SIGNAL( notificationClosed(Snore::Notification) ), this, SLOT( slotNotificationClosed( Snore::Notification) ) );
}

WrenchMainWindow::~WrenchMainWindow()
{
    deleteLuaThread();
    delete ui;
}

Snore::Icon WrenchMainWindow::getPkgIcon(const QString& pkg)
{
    if (m_pkg_icons.contains(pkg)) {
        return m_pkg_icons.value(pkg, m_defaultIcon);
    }

    QStringList imgs = gDataDir.entryList(QStringList(pkg + ".*.png"));
    if (!imgs.empty()) {
        QPixmap pixmap(gDataDir.absoluteFilePath(imgs[0]));
        Snore::Icon icon(pixmap);
        m_pkg_icons.insert(pkg, icon);
        return icon;
    }
    return m_defaultIcon;
}

void WrenchMainWindow::onAdbNotificationArrived(const QString& key, const QString& pkg, const QString& title, const QString& text, const QString& ticker)
{
    if (pkg == "WrenchNotificationResult") {
        if (key == mLastClickedNotification["key"]) {
            addLog("No result handling notification, use shift click");
            adbNotificationShiftClicked(mLastClickedNotification);
        } else {
            addLog("Don't know how to handle notification: " + key);
        }
        return;
    }
    if (mWrenchExt.isUsefulNotification(key, pkg, title, text, ticker)) {
        QString newText = mWrenchExt.reWriteNotificationText(key, pkg, title, text, ticker);
        if (!mLuaThread.isNull()) {
            mLuaThread->addScript(QStringList() << "notification_arrived" << key << pkg << title << newText);
        }

        NotificationModel::insertNotification(key, pkg, title, newText);

        if (mWrenchExt.shouldUseInternalPop()) {
            Snore::Notification n(m_snore_application, m_alert, title, newText, getPkgIcon(pkg));
            m_notification_map.insert(n.id() % max_notification_entries, key);
            m_notification_map.insert(n.id() % max_notification_entries + max_notification_entries, pkg);
            m_notification_map.insert(n.id() % max_notification_entries + max_notification_entries * 2, title);
            m_notification_map.insert(n.id() % max_notification_entries + max_notification_entries * 3, newText);
            m_snore->broadcastNotification(n);
            m_last_shown_notification_id = n.id();
            m_last_notification = n;
        }
    }
}

void WrenchMainWindow::adbStateUpdated(const QString& state)
{
    if (state.contains("notification-", Qt::CaseInsensitive)) {
        if (state.contains("-online", Qt::CaseInsensitive)) {
            mNotificationOnline = true;
        } else {
            mNotificationOnline = false;
        }
        if (mInputOnline) {
            adbStateUpdated("Online");
        } else {
            adbStateUpdated("Offline");
        }
        return;
    }

    if (state.toLower() == "online") {
        mInputOnline = true;
    } else {
        mInputOnline = false;
    }

    if (mInputOnline && ui->adbStateLabel->text().toLower() != "online") {
        if (!mLuaThread.isNull() && mLuaThread->isRunning()) {
            mLuaThread->addScript(QStringList() << "wrench_config" << gConfigDir.absolutePath());
        } else {
            on_configurePushButton_clicked();
        }
    }
    ui->adbStateLabel->setText(state);

    if (mInputOnline) {
        if (mNotificationOnline) {
            ui->adbStateIndicator->setIcon(QIcon("emojis/iphone-new/BELL.png"));
        } else {
            ui->adbStateIndicator->setIcon(QIcon("emojis/iphone-new/BELL_WITH_CANCELLATION_STROKE.png"));
        }
    } else {
        ui->adbStateIndicator->setIcon(QIcon(":/images/red.png"));

        qint64 currentSec = QDateTime::currentMSecsSinceEpoch();
        static qint64 lastSec;
        if (currentSec - lastSec > 5000) {
            QProcess::startDetached("./the-true-adb", QStringList("start-server"));
            lastSec = currentSec;
        }

    }
}

// static void appendCmdOutput(QTextEdit *cmdOutputEdit, const QString& output)
// {
//     cmdOutputEdit->moveCursor(QTextCursor::End);
//     cmdOutputEdit->insertPlainText(output);
// }

QString prompt_user(const QString &info, QMessageBox::StandardButtons buttons)
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

QString WrenchMainWindow::get_text()
{
    QString text = ui->phoneTextEdit->getMyText();
    while (text.endsWith("\r") || text.endsWith("\n") || text.endsWith("\t") ||
           text.endsWith(" ")) {
        text = text.left(text.length() -1);
    }
    return text;
}

void WrenchMainWindow::addLog(const QString& log)
{
    onInfoUpdate("log", log);
}

void WrenchMainWindow::onInfoUpdate(const QString& key, const QString& val)
{
    static int nTasks;

    if (key == "getclip-android") {
        ui->phoneTextEdit->insertPlainText(val);
    } else if (key == "prompt") {
        prompt_user(val);
    } else if (key == "exit") {
        if (yes_or_no_p(QString().sprintf("Lua crashed in the background, restart it?\n\n    %s", qPrintable(val))) == "yes") {
            on_configurePushButton_clicked();
        }
    } else if (key == "") {
        // do nothing.
    } else if (key == "start task") {
        ui->adbStateLabel->setText(QString().sprintf("Tasks: %d", ++nTasks));
    } else if (key == "end task") {
        ui->adbStateLabel->setText(QString().sprintf("Tasks: %d", --nTasks));
    } else {
        static int lines = 0;
        if (lines > 1000) {
            ui->cmdOutputEdit->clear();
            lines = 0;
        }

        static QString lastKey, lastVal;

        ui->cmdOutputEdit->moveCursor(QTextCursor::End);
        QString v = val;
        if (val.isEmpty()) {
            v = "OK";
        }

        if (lastKey == key && lastVal == val) {

        } else {
            ui->cmdOutputEdit->insertPlainText(key + ": " + v + "\n");
        }

        lastKey = key;
        lastVal = val;
        lines++;
    }
}

QString WrenchMainWindow::selectArgs(const QString& prompt, const QStringList &args)
{
    switch (args.size()) {
    case 1:
        return args[0];
    case 0:
        return "";
    }

    StrlistModel model(args);
    DialogGetEntry dialog(&model, prompt, this);
    dialog.setOneShot();
    dialog.exec();
    return dialog.mSelectedEntry;
}

void WrenchMainWindow::onSelectArgsForLua(const QStringList& args)
{
    if (args.size() == 1) {
        if (yes_or_no_p(args[0]) == "yes") {
            mLuaThread->on_argSelected(args[0]);
        } else {
            mLuaThread->on_argSelected("");
        }
        return;
    }

    QString prompt = args[0];
    QStringList argsCopy = args;
    argsCopy.pop_front();
    StrlistModel model(argsCopy);

    DialogGetEntry dialog(&model, prompt, this);
    dialog.setOneShot();

    dialog.exec();
    mLuaThread->on_argSelected(dialog.mSelectedEntry);
}

void WrenchMainWindow::onSelectApps()
{
    AppsModel model;
    QString prompt = "Select the app to launch";

    DialogGetEntry dialog(&model, prompt, this);
    connect(&dialog, SIGNAL(entrySelected(QString)), this, SLOT(on_appSelected(QString)));
    dialog.exec();
    dialog.disconnect();
}

void WrenchMainWindow::onShowNotifications()
{
    NotificationModel model;
    QString prompt = "Select whose notifications to show";

    DialogGetEntry dialog(&model, prompt, this);
    connect(&dialog, SIGNAL(entrySelected(QString)), this, SIGNAL(adbNotificationClicked(QString)));
    connect(&dialog, SIGNAL(selectRawData(QMap<QString, QString>)), this, SLOT(adbNotificationShiftClicked(QMap<QString, QString>)));
    connect(&model, SIGNAL(saveLastDialogClickedNotification(QMap<QString, QString>)), this, SLOT(saveLastDialogClickedNotification(QMap<QString, QString>)));
    dialog.exec();
    model.disconnect();
    dialog.disconnect();
}

void WrenchMainWindow::saveLastDialogClickedNotification(const QMap<QString, QString>& notification)
{
    mLastClickedNotification = notification;
}

bool WrenchMainWindow::anyShareChecked()
{
    return  ui->tbWeibo->isChecked() ||
        ui->tbWeixin->isChecked() ||
        ui->tbQq->isChecked();
}

void WrenchMainWindow::on_sendItPushButton_clicked()
{
    QString text = get_text();
    if (text.isEmpty() && mPictures.isEmpty()) {
        prompt_user("Composed text must not be empty", QMessageBox::Ok);
        return;
    }

    ui->phoneTextEdit->setPlaceholderText("");

    bool share = 0;
    if (anyShareChecked() && ! mPictures.isEmpty()) {
        mLuaThread->addScript(QStringList("upload_pics") + mPictures);
    }
    if (ui->tbWeibo->isChecked()) {
        share = 1;
        if (mPictures.isEmpty()) {
            mLuaThread->addScript(QStringList() << "wrench_share_to_weibo" << text);
        } else {
            mLuaThread->addScript(QStringList() << "picture_to_weibo_share");
            mLuaThread->addScript(QStringList() << "wrench_post" << text);
        }
        ui->tbWeibo->setChecked(false);
    }

    if (ui->tbWeixin->isChecked()) {
        share = 1;
        if (mPictures.isEmpty()) {
            mLuaThread->addScript(QStringList() << "wrench_share_to_weixin" << text);
        } else {
            mLuaThread->addScript(QStringList() << "picture_to_weixin_share");
            mLuaThread->addScript(QStringList() << "wrench_post" << text << "top-right" << "确认分享到微信？");
        }
        ui->tbWeixin->setChecked(false);
    }

    if (ui->tbQq->isChecked()) {
        share = 1;
        if (mPictures.isEmpty()) {
            mLuaThread->addScript(QStringList() << "wrench_share_to_qq" << text);
        } else {
            mLuaThread->addScript(QStringList() << "picture_to_qq_share");
            mLuaThread->addScript(QStringList() << "wrench_post" << text);
        }
        ui->tbQq->setChecked(false);
    }

    if (share && !mPictures.isEmpty()) {
        mPictures.clear();
        ui->tbPicture->setChecked(false);
        ui->tbScreenCapture->setChecked(false);
    }

    if (! share) {
        mLuaThread->addScript(QStringList() << "wrench_post" << text);
    }
    ui->phoneTextEdit->selectAll();
}

void WrenchMainWindow::deleteLuaThread()
{
    if (mLuaThread.isNull()) {
        return;
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

    if (mLuaThread->isRunning()) {
        qDebug() << "it is still running";
        mLuaThread->terminate();
    }
    mLuaThread.clear();

}
void WrenchMainWindow::on_configurePushButton_clicked()
{
    bool is_starting = false;
    if (mLuaThread.isNull()) {
        is_starting = true;
    }
    mWrenchExt.reloadLuaScript();
    QProcess::startDetached("./the-true-adb", QStringList("start-server"));
    if (!mLuaThread.isNull()) {
        if (mLuaThread->isRunning()) {
            if (yes_or_no_p("Lua is still running in the background, click OK to re-configure, click Cancel to continue to use current script.") != "yes") {
                return;
            }
        }
        deleteLuaThread();
    }
    mLuaThread = QSharedPointer<LuaExecuteThread>(new LuaExecuteThread(this));
    connect(mLuaThread.data(), SIGNAL(gotSomeLog(QString, QString)), this, SLOT(onInfoUpdate(QString, QString)));
    if (!mPhoneScreenDialog.isNull()) {
        this->connect(mLuaThread.data(), SIGNAL(requestSyncScreen()), mPhoneScreenDialog.data(), SLOT(syncScreen()), Qt::QueuedConnection);
    }
    connect(mLuaThread.data(), SIGNAL(selectArgsSig(QStringList)), this, SLOT(onSelectArgsForLua(QStringList)));
    connect(mLuaThread.data(), SIGNAL(selectAppsSig()), this, SLOT(onSelectApps()));
    connect(mLuaThread.data(), SIGNAL(showNotificationsSig()), this, SLOT(slotShortCutActivated()));
    connect(mLuaThread.data(), SIGNAL(sigClickNotification(QString)), this, SIGNAL(adbNotificationClicked(QString)));
    connect(mLuaThread.data(), SIGNAL(load_mail_heads_sig(QString, QString, QString, QString, QString)), this, SLOT(onLoadMailHeads(QString, QString, QString, QString, QString)));
    mLuaThread->start();
    if (! is_starting) {
        mLuaThread->addScript(QStringList() << "wrench_config" << gConfigDir.absolutePath());
    }
}

void WrenchMainWindow::on_tbScreenCapture_clicked()
{
    prompt_user("Screen capture used some Linux Ali Wangwang's closed source code, and is removed for the moment");
}

void WrenchMainWindow::slotHandleCaptureScreen(const QPixmap &pix)
{
    QObject::sender()->deleteLater();
    //mScreenCapture.clear();
    if (pix.isNull()) return;

    pix.save(gDataDir.absoluteFilePath("screen-shot.png"), "PNG");

    if (anyShareChecked() &&
        ui->tbScreenCapture->isChecked() &&
        !mPictures.isEmpty()) {
        ui->tbScreenCapture->setChecked(false);
        mPictures.removeOne(gDataDir.absoluteFilePath("screen-shot.png"));
        return;
    }

    if (anyShareChecked()) {
        QString text = get_text();
        ui->tbScreenCapture->setCheckable(true);
        ui->tbScreenCapture->setChecked(true);
        if (text.isEmpty()) {
            if (yes_or_no_p("No more to say？\n\nClick OK to share, click Cancel to compose some text and Send") != "yes") {
                return;
            }
        }
        mPictures = QStringList() << gDataDir.absoluteFilePath("screen-shot.png");
        emit ui->sendItPushButton->clicked();
    } else {
        mLuaThread->addScript(QStringList() << "wrench_picture" << gDataDir.absoluteFilePath("screen-shot.png"));
    }
}

void WrenchMainWindow::on_tbPicture_clicked()
{
    if (anyShareChecked() && !mPictures.isEmpty()) {
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

    sharePictures(fns);
}

void WrenchMainWindow::sharePictures(const QStringList& files)
{
        if (anyShareChecked()) {
        QString text = get_text();
        ui->tbPicture->setCheckable(true);
        ui->tbPicture->setChecked(true);
        if (text.isEmpty()) {
            if (yes_or_no_p("No more to say？\n\nClick OK to share, click Cancel to compose some text and Send") != "yes") {
                return;
            }
        }
        mPictures = files;
        emit ui->sendItPushButton->clicked();
    } else {
        mLuaThread->addScript((QStringList() << "wrench_picture") + files);
    }
}

void WrenchMainWindow::on_tbEmoji_clicked()
{
    if (mEmojiDialog.isNull()) {
        mEmojiDialog = QSharedPointer<DialogGetEntry>(new DialogGetEntry(new EmojiModel(0), "表情过滤", this, true));
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

void WrenchMainWindow::on_tbWeibo_clicked()
{
    if (!anyShareChecked()) {
        ui->tbPicture->setCheckable(false);
        mPictures.clear();
    }
}

void WrenchMainWindow::on_tbWeixin_clicked()
{
    if (!anyShareChecked()) {
        ui->tbPicture->setCheckable(false);
        mPictures.clear();
    }
}

void WrenchMainWindow::on_tbQq_clicked()
{
    if (!anyShareChecked()) {
        ui->tbPicture->setCheckable(false);
        mPictures.clear();
    }
}

void WrenchMainWindow::on_tbThumbsUp_clicked()
{
    mLuaThread->addScript(QStringList() << "wrenchThumbUp");
}

void WrenchMainWindow::initContactDialog(bool isMail)
{
    QString placeHolder = "联系人过滤";
    if (mContactDialog.isNull()) {
        mContactModel = new ContactModel(0);
        mContactDialog = QSharedPointer<DialogGetEntry>(new DialogGetEntry(mContactModel, placeHolder, this));
    }
    mContactModel->setInitFilter("");
    mContactDialog->setHint(placeHolder);
    mContactModel->setMail(isMail);
}

void WrenchMainWindow::on_tbMms_clicked()
{
    if (ui->tbWeixin->isChecked()) {
        ui->tbWeixin->setChecked(false);
    }
    if (ui->tbQq->isChecked()) {
        ui->tbQq->setChecked(false);
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
    mLuaThread->addScript(QStringList() << "wrench_add_mms_receiver" << receivers);
}

void WrenchMainWindow::afterUsingContactDialog()
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
void WrenchMainWindow::on_tbPhoneCall_clicked()
{

    initContactDialog();
    connect(mContactDialog.data(), SIGNAL(entrySelected(QString)), this, SLOT(on_Dial(QString)));
    afterUsingContactDialog();
    ui->tbWeixin->setChecked(false);
    ui->tbQq->setChecked(false);
}

void WrenchMainWindow::on_addMmsReceiver(const QString&contact, const QString& display)
{
    if (mMmsReceiverMap.contains(contact) && yes_or_no_p(mMmsReceiverMap[contact] +
                                                         "Already in SMS receiver, delete？") == "yes") {
        mMmsReceiverMap.remove(contact);
        return;
    }
    onInfoUpdate("info", "Send SMS to " + display);
    mMmsReceiverMap[contact] = display;
}

void WrenchMainWindow::on_Dial(const QString&contact)
{
    if (ui->tbWeixin->isChecked()) {
        mLuaThread->addScript(QStringList() << "wrench_find_weixin_contact" << contact);
        return;
    }
    if (ui->tbQq->isChecked()) {
        mLuaThread->addScript(QStringList() << "wrench_find_qq_contact" << contact);
        return;
    }
    mLuaThread->addScript(QStringList() << "wrench_call" << contact);
}

void WrenchMainWindow::quitMyself()
{
    mQuit = true;
    wrenchSetQuitting();
    close();
}

void WrenchMainWindow::createTrayIcon()
{

    quitAction = new QAction(tr("&Quit"), this);
    connect(quitAction, SIGNAL(triggered()), this, SLOT(quitMyself()));

    showNotificationAction = new QAction(tr("&Show notifications"), this);
    connect(showNotificationAction, SIGNAL(triggered()), this, SLOT(onShowNotifications()));

    trayIconMenu = new QMenu(this);
    trayIconMenu->addAction(quitAction);
    trayIconMenu->addAction(showNotificationAction);


    trayIcon = new QSystemTrayIcon(this);
    trayIcon->setContextMenu(trayIconMenu);

    QIcon icon("android-wrench.png");
    trayIcon->setIcon(icon);

    connect(trayIcon, SIGNAL(activated(QSystemTrayIcon::ActivationReason)),
            this, SLOT(iconActivated(QSystemTrayIcon::ActivationReason)));
    trayIcon->show();
}

void WrenchMainWindow::iconActivated(QSystemTrayIcon::ActivationReason reason)
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

void WrenchMainWindow::closeEvent(QCloseEvent *event)
{
    if (mQuit == true) {
        if (!mPhoneScreenDialog.isNull()) {
            mPhoneScreenDialog->close();
        }

        if (vncMainWindow) {
            vncMainWindow->close();
        }
        QMainWindow::closeEvent(event);
        return;
    }

    if (trayIcon->isVisible()) {
        hide();
        if (mPhoneScreenDialog) {
            mPhoneScreenDialog->hide();
        }

        if (vncMainWindow) {
            vncMainWindow->hide();
        }
        event->ignore();
    }
}

void WrenchMainWindow::showEvent(QShowEvent *event)
{
    if (mPhoneScreenDialog && ui->tbPhoneScreen->isChecked()) {
        mPhoneScreenDialog->show();
    }

    if (vncMainWindow && ui->tbPhoneScreen->isChecked()) {
        vncMainWindow->show();
        moveVncMainWin();
    }

}

void WrenchMainWindow::startTask(const QString& task)
{
    qDebug() << qPrintable(QString().sprintf("%s:%d:", __FILE__, __LINE__));
    showNormal();
    emit activateWindow();

    QString path = task;
    QFileInfo fileInfo = QFileInfo(path);
    if (fileInfo.isReadable() && fileInfo.isFile()) {
        mLuaThread->addScript(QStringList() << "wrench_run" << path);
    }

}

void WrenchMainWindow::dragEnterEvent(QDragEnterEvent *event)
{
    event->acceptProposedAction();
}

static bool isCursorOverIt(const QWidget* it)
{
    return it->rect().contains(it->mapFromGlobal(QCursor::pos()));
}

void WrenchMainWindow::dropEvent(QDropEvent *event)
{
    if (isCursorOverIt(ui->tbWeibo)) {
        if (event->mimeData()->hasUrls()) {
            QList<QUrl> urls = event->mimeData()->urls();
            if (urls.size() == 1) {
                mLuaThread->addScript(QStringList() << "call_ext" << "open-weibo" << urls[0].toString());
                return;
            }
        }
    }

    if (isCursorOverIt(ui->tbThumbsUp)) {
        if (event->mimeData()->hasText()) {
            mLuaThread->addScript(QStringList() << "set_ext_args" << event->mimeData()->text());
            mLuaThread->addScript(QStringList() << "wrenchThumbUp");
            mLuaThread->addScript(QStringList() << "set_ext_args");
            return;
        }
    }

    if (ui->adbStateIndicator->rect().contains(ui->adbStateIndicator->mapFromGlobal(QCursor::pos()))) {
        if (event->mimeData()->hasText()) {
            mLuaThread->addScript(QStringList() << "call_ext" << "GTD-Capture" << event->mimeData()->text());
            return;
        }
    }

    event->acceptProposedAction();
    qDebug() << "WrenchMainWindow::dropEvent";

    if (isCursorOverIt(ui->tbQq)) {
        mLuaThread->addScript(QStringList() << "wrench_call" << "@@qq");
    } else if (isCursorOverIt(ui->tbWeixin)) {
        mLuaThread->addScript(QStringList() << "wrench_call" << "@@wx");
    }

    if (event->mimeData()->hasImage()) {
        imageDropped(*event);
        qDebug() << "WrenchMainWindow::dropEvent" << "hasImage";
        return;
    }
    if (event->mimeData()->hasUrls()) {
        QList<QUrl> urls = event->mimeData()->urls();
        if (urls.size() == 1 && urls[0].isLocalFile()) {
            QFileInfo fi(urls[0].toLocalFile());
            QString suf = fi.suffix();
            if (suf == "twa" || suf == "lua") {
                startTask(urls[0].toLocalFile());
                return;
            }
        }

        if (urls.size()) {
            imageDropped(*event);
        }
    }
}

void WrenchMainWindow::on_appSelected(const QString& app)
{
    mLuaThread->addScript(QStringList() << "on_app_selected" << app);
}

void WrenchMainWindow::on_tbMailAddTo_clicked()
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

void WrenchMainWindow::on_MailTo(const QString& to)
{
    addMailContact(to, ui->ptMailTo);
}

void WrenchMainWindow::on_tbMailAddCc_clicked()
{
    initContactDialog(true);
    connect(mContactDialog.data(), SIGNAL(entrySelected(QString)), this, SLOT(on_MailCc(QString)));
    afterUsingContactDialog();
}

void WrenchMainWindow::on_MailCc(const QString& to)
{
    addMailContact(to, ui->ptMailCc);
}

void WrenchMainWindow::on_tbMailAddBcc_clicked()
{
    initContactDialog(true);
    connect(mContactDialog.data(), SIGNAL(entrySelected(QString)), this, SLOT(on_MailBcc(QString)));
    afterUsingContactDialog();
}

void WrenchMainWindow::on_MailBcc(const QString& to)
{
    addMailContact(to, ui->ptMailBcc);
}

void WrenchMainWindow::on_tbMailAddAttachment_clicked()
{
    QStringList fns = QFileDialog::getOpenFileNames(this, tr("选择附件"), QString(), tr("All Files(*)"));
    if (fns.isEmpty()) {
        return;
    }
    foreach (const QString& file, fns) {
        ui->ptMailAttachments->setPlainText(ui->ptMailAttachments->toPlainText() + file + "\n");
    }
}

static QStringList getMailHeads(Ui::WrenchMainWindow* ui)
{
    return QStringList() << ui->ptMailSubject->toPlainText()
                         << ui->ptMailTo->toPlainText()
                         << ui->ptMailCc->toPlainText()
                         << ui->ptMailBcc->toPlainText()
                         << ui->ptMailAttachments->toPlainText();
}

void WrenchMainWindow::on_tbMailDone_clicked()
{
    ui->tabWidget->setCurrentIndex(0);
    ui->phoneTextEdit->setFocus(Qt::OtherFocusReason);
    mLuaThread->addScript((QStringList() << "wrench_adb_mail") + getMailHeads(ui));
}

void WrenchMainWindow::on_tbMailLoad_clicked()
{
    QString file = QFileDialog::getOpenFileName(0, "请选择打开哪个邮件小扳手脚本文件", QString(), "小扳手脚本文件(*.twa)");
    mLuaThread->addScript(QStringList() << "wrench_run" << file);
}

void WrenchMainWindow::on_tbMailSave_clicked()
{
    QString file = QFileDialog::getSaveFileName(0, "请选择保存常用联系方式到哪个文件", QString(), "小扳手脚本文件(*.twa)");
    mLuaThread->addScript((QStringList() << "wrench_save_mail_heads" << file) + getMailHeads(ui));
}

void WrenchMainWindow::on_tbMailClear_clicked()
{
    onLoadMailHeads("", "", "", "", "");
}

void WrenchMainWindow::onLoadMailHeads(const QString& subject, const QString& to, const QString& cc, const QString& bcc, const QString& attachments)
{
    ui->ptMailSubject->setPlainText(subject);
    ui->ptMailTo->setPlainText(to);
    ui->ptMailCc->setPlainText(cc);
    ui->ptMailBcc->setPlainText(bcc);
    ui->ptMailAttachments->setPlainText(attachments);
    ui->tabWidget->setCurrentIndex(1);
}

void WrenchMainWindow::moveVncMainWinWhenMoving()
{
    QRect rectWithFrame = this->frameGeometry();
    movePhoneScreenWindowXY(rectWithFrame.right(), rectWithFrame.top());
}

void WrenchMainWindow::moveVncMainWin()
{
    QRect rectWithFrame = this->frameGeometry();
    QRect rectNoFrame = this->geometry();

    int xDelta = 0;
    int yDelta = 0;
    if (QProcessEnvironment::systemEnvironment().value("DESKTOP_SESSION") == "sawfish") {
        xDelta  = rectWithFrame.right() - rectNoFrame.right();
        yDelta = rectNoFrame.top() - rectWithFrame.top();
    }
    movePhoneScreenWindowXY(rectWithFrame.right() + xDelta, rectWithFrame.top() + yDelta);
}

void WrenchMainWindow::movePhoneScreenWindowXY(int x, int y)
{
    if (vncMainWindow && vncMainWindow->isVisible())
        vncMainWindow->move(x, y);

    if (!mPhoneScreenDialog.isNull() && mPhoneScreenDialog->isVisible())
        mPhoneScreenDialog->move(x, y);
}

volatile bool gPhoneScreenSyncOn;
void WrenchMainWindow::on_tbPhoneScreen_toggled(bool checked)
{
    QString usingVnc = mLuaThread->getVariableLocked("using-vnc");
    if (usingVnc.isEmpty()) {
        prompt_user("Have not decided whether you have vnc, using screencapture");
    }
    if (usingVnc == "true") {
        if (!mPhoneScreenDialog.isNull() && mPhoneScreenDialog->isVisible()) {
            mPhoneScreenDialog->hide();
        }
        static AdbVncThread* vncThread;
        if (checked) {
            if (vncThread == NULL) {
                vncThread = new AdbVncThread();
                vncThread->moveToThread(vncThread);
                vncThread->start();
            }

            if (vncMainWindow == NULL) {
                vncMainWindow = new VncMainWindow(this);

                vncMainWindow->installEventFilter(vncMainWindow);
            }

            vncMainWindow->show();
            moveVncMainWin();

            connect(vncThread, SIGNAL(adbVncUpdate(QString)), vncMainWindow, SLOT(onVncUpdate(QString)));

        } else if (vncMainWindow) {
            mLuaThread->addScript(QStringList("kill_android_vnc"));
            vncMainWindow->hide();
        }
        return;
    }

    if (checked) {
        if (mPhoneScreenDialog.isNull()) {
            mPhoneScreenDialog = QSharedPointer<PhoneScreenDialog>(new PhoneScreenDialog(this));
            mPhoneScreenDialog->setModal(false);
            mPhoneScreenDialog->setFixedSize(this->size().height() * 1187 / 2457, this->size().height());
            int winId = mPhoneScreenDialog->winId();
            mPhoneScreenDialog->installEventFilter(mPhoneScreenDialog.data());
            this->connect(mLuaThread.data(), SIGNAL(requestSyncScreen()), mPhoneScreenDialog.data(), SLOT(syncScreen()), Qt::QueuedConnection);
        }
        mPhoneScreenDialog->show();
        moveVncMainWin();
    } else {
        mPhoneScreenDialog->hide();
    }
}

bool WrenchMainWindow::eventFilter(QObject *obj, QEvent *ev)
{
    if (strcmp(obj->metaObject()->className(), "QToolButton") == 0) {
        if (ev->type() == QEvent::KeyPress) {
            QKeyEvent* e = (QKeyEvent*) ev;
            if (e->key() == Qt::Key_Escape && ui->tabWidget->currentIndex() == 0) {
                ui->phoneTextEdit->setFocus(Qt::OtherFocusReason);
                return true;
            }
        }
    } else if (ev->type() == QEvent::KeyPress) {
        QWidget* w = (QWidget *)obj;
        QKeyEvent* e = (QKeyEvent*)ev;
        return handleEmacsKeys(w, e);
    }

    return QMainWindow::eventFilter(obj, ev);
}

bool WrenchMainWindow::handleEmacsKeys(QWidget *w, QKeyEvent *e)
{
    int key = e->key();
    Qt::KeyboardModifiers m = e->modifiers();
    const Qt::KeyboardModifiers generalMods = Qt::ShiftModifier | Qt::ControlModifier | Qt::AltModifier | Qt::MetaModifier;

    static bool last_is_escape = false;
    typedef struct {
        int from;
        Qt::KeyboardModifiers mod_from;
        int to;
        Qt::KeyboardModifiers mod_to;
    } single_keymap_t;

    static single_keymap_t single_map[] = {
        { Qt::Key_B, Qt::ControlModifier, Qt::Key_Left, 0, },
        { Qt::Key_F, Qt::ControlModifier, Qt::Key_Right, 0, },
        { Qt::Key_P, Qt::ControlModifier, Qt::Key_Up, 0, },
        { Qt::Key_N, Qt::ControlModifier, Qt::Key_Down, 0, },
        { Qt::Key_A, Qt::ControlModifier, Qt::Key_Home, 0, },
        { Qt::Key_G, Qt::ControlModifier, Qt::Key_Escape, 0, },
        { Qt::Key_D, Qt::ControlModifier, Qt::Key_Delete, 0, },
        { Qt::Key_E, Qt::ControlModifier, Qt::Key_End, 0, },
        { Qt::Key_Y, Qt::ControlModifier, Qt::Key_Insert, Qt::ShiftModifier, },
        { Qt::Key_V, Qt::ControlModifier, Qt::Key_PageDown, 0, },
        { Qt::Key_V, Qt::AltModifier,     Qt::Key_PageUp, 0, },
        { Qt::Key_E, Qt::ControlModifier, Qt::Key_End, 0, },
        { Qt::Key_B, Qt::AltModifier,     Qt::Key_Left, Qt::ControlModifier, },
        { Qt::Key_F, Qt::AltModifier,     Qt::Key_Right, Qt::ControlModifier, },
        { Qt::Key_B, Qt::AltModifier | Qt::ShiftModifier, Qt::Key_Left, Qt::ControlModifier | Qt::ShiftModifier, },
        { Qt::Key_F, Qt::AltModifier | Qt::ShiftModifier, Qt::Key_Right, Qt::ControlModifier | Qt::ShiftModifier, },
        { Qt::Key_Less, Qt::AltModifier | Qt::ShiftModifier, Qt::Key_Home, Qt::ControlModifier, },
        { Qt::Key_Greater, Qt::AltModifier | Qt::ShiftModifier, Qt::Key_End, Qt::ControlModifier, },
        { Qt::Key_Backspace, Qt::AltModifier, Qt::Key_Backspace, Qt::ControlModifier, },
    };

    typedef struct {
        int from;
        Qt::KeyboardModifiers mod_from;
        struct {
            int to;
            Qt::KeyboardModifiers mod_to;
        } mto[5];
    } multi_keymap_t;

    static multi_keymap_t multi_map[] = {
        { Qt::Key_K, Qt::ControlModifier,
          { { Qt::Key_End, Qt::ShiftModifier, },
            { Qt::Key_Delete, 0, },
          },
        },

        { Qt::Key_D, Qt::AltModifier,
          {
              { Qt::Key_Right, Qt::ControlModifier | Qt::ShiftModifier, },
              { Qt::Key_Backspace, },
          },
        },
    };

    if (last_is_escape) {
        if (key == Qt::Key_Shift || key == Qt::Key_Meta || key == Qt::Key_Control || key == Qt::Key_Alt) {
            true;
        } else {
            last_is_escape = false;
        }
        if (m & Qt::AltModifier) {
            m &= ~Qt::AltModifier;
        } else {
            m |= Qt::AltModifier;
        }
    } else if (m == 0 && key == Qt::Key_Escape) {
        last_is_escape = true;
        return true;
    }

    for (size_t i = 0; i < sizeof(single_map) / sizeof(single_map[0]); i++) {
        if (m == single_map[i].mod_from)
        if (key == single_map[i].from && m == single_map[i].mod_from) {
            QKeyEvent *nkey = new QKeyEvent(e->type(), single_map[i].to, single_map[i].mod_to);
            QCoreApplication::postEvent(w, nkey);
            return true;
        }
    }
    for (size_t i = 0; i < sizeof(multi_map) / sizeof(multi_map[0]); i++) {
        if (key == multi_map[i].from && m == multi_map[i].mod_from) {
            for (int j = 0; multi_map[i].mto[j].to; j++) {
                QKeyEvent *nkey = new QKeyEvent(e->type(), multi_map[i].mto[j].to, multi_map[i].mto[j].mod_to);
                QCoreApplication::postEvent(w, nkey);
            }
            return true;
        }
    }

    if (m == (Qt::AltModifier | Qt::ControlModifier)) {
        if (key == Qt::Key_Q) {
            this->ui->tbQq->toggle();
        } else if (key == Qt::Key_B) {
            this->ui->tbWeibo->toggle();
        } else if (key == Qt::Key_W) {
            this->ui->tbWeixin->toggle();
        } else {
            goto default_filter;
        }
        return true;
    }

default_filter:
    return QMainWindow::eventFilter(w, e);
}

void WrenchMainWindow::moveEvent(QMoveEvent* ev)
{
    if (vncMainWindow && vncMainWindow->isVisible() ||
        !mPhoneScreenDialog.isNull() && mPhoneScreenDialog->isVisible()) {
        QTimer::singleShot(100, this, SLOT(moveVncMainWinWhenMoving()));
    }
}

void WrenchMainWindow::on_tbLauncher_clicked()
{
    mLuaThread->addScript(QStringList() << "launch_apps");
}

void WrenchMainWindow::saveLastClickedSnoreNotification(uint n_key)
{
    mLastClickedNotification.insert("key", m_notification_map[n_key]);
    mLastClickedNotification.insert("pkg", m_notification_map[n_key + max_notification_entries]);
    mLastClickedNotification.insert("title", m_notification_map[n_key + max_notification_entries * 2]);
    mLastClickedNotification.insert("text", m_notification_map[n_key + max_notification_entries * 3]);
}

void WrenchMainWindow::clickLastSnoreNotification()
{
    mLuaThread->addScript(QStringList()
                          << "wrench_click_notification"
                          << mLastClickedNotification["key"]
                          << mLastClickedNotification["pkg"]
                          << mLastClickedNotification["title"]
                          << mLastClickedNotification["text"]
        );
}

void WrenchMainWindow::slotNotificationClosed( Snore::Notification n)
{
    qDebug() << "close notification" << n.closeReason();

    uint n_key = n.id() % max_notification_entries;

    QString key = m_notification_map[n_key];
    if (key.isEmpty())
        return;


    m_last_closed_notification_id = n.id();

    if (n.closeReason() == Snore::Notification::CloseReasons::Dismissed ||
        n.closeReason() == Snore::Notification::CloseReasons::Activated) {
        if (!key.isEmpty()) {
            saveLastClickedSnoreNotification(n_key);
            clickLastSnoreNotification();
        }
    }
    m_notification_map[n.id() % max_notification_entries] = "";
}

void WrenchMainWindow::slotShortCutActivated()
{
    if (m_last_shown_notification_id == m_last_closed_notification_id + 1) {
        uint n_key = m_last_shown_notification_id % max_notification_entries;
        QString key = m_notification_map[n_key];
        if (!key.isEmpty()) {
            saveLastClickedSnoreNotification(n_key);
            clickLastSnoreNotification();
            m_snore->requestCloseNotification(m_last_notification, Snore::Notification::CloseReasons::None);
        }
    } else {
        onShowNotifications();
    }
}

void WrenchMainWindow::adbNotificationShiftClicked(const QMap<QString, QString>& rawData)
{
    mLuaThread->addScript(QStringList({"shift_click_notification", rawData["key"], rawData["pkg"], rawData["title"], rawData["text"]}));
}

void WrenchMainWindow::imageDropped(const QDropEvent& ev)
{
    qDebug() << __FUNCTION__;

    const QDropEvent* event = &ev;
    QList<QUrl> urls = event->mimeData()->urls();
    if (urls.count()) {
        QUrl url = urls[0];
        if (url.isLocalFile()) { // if first is local, suppose all are local
            QStringList files;
            foreach(const QUrl& u, urls) {
                if (u.isLocalFile())
                    files << u.toLocalFile();
            }
            sharePictures(files);
        } else {
            qDebug() << "imageDropped got " << url;
            m_manager.get(QNetworkRequest(url));
        }
    } else if (event->mimeData()->hasImage()) {
        QImage image = qvariant_cast<QImage>(event->mimeData()->imageData());
        image.save(gDataDir.absoluteFilePath("drag-and-drop.jpg"));
        sharePictures(QStringList() << gDataDir.absoluteFilePath("drag-and-drop.jpg"));
    }
}

void WrenchMainWindow::handleNetworkData(QNetworkReply *networkReply)
{
    QImage image;
    QUrl url = networkReply->url();

    qDebug() << "Received" << networkReply->size() << "bytes" << "from" << url.fileName();

    if (networkReply->error()) {
        prompt_user(QString("Can't download ") + url.toString());
        return;
    } else {
        if (url.toString().endsWith(".gif", Qt::CaseInsensitive)) {
            QByteArray gifData = networkReply->readAll();
            QFile gifFile(gDataDir.absoluteFilePath("drag-and-drop.gif"));
            if (!gifFile.open(QIODevice::WriteOnly)) {
                prompt_user("can't open " + gDataDir.absoluteFilePath("drag-and-drop.gif"));
                return;
            }
            if (gifFile.write(gifData) != gifData.size()) {
                prompt_user("Failed to write all gif data");
                return;
            }
            gifFile.close();
            sharePictures(QStringList() << gDataDir.absoluteFilePath("drag-and-drop.gif"));
        } else {
            image.load(networkReply, 0);
            image.save(gDataDir.absoluteFilePath("drag-and-drop.jpg"));
            sharePictures(QStringList() << gDataDir.absoluteFilePath("drag-and-drop.jpg"));
        }
    }
}

void WrenchMainWindow::on_adbStateIndicator_clicked()
{
    if (! mInputOnline) {

        QStringList adb_devices = AdbClient::doAdbDevices();
        if (adb_devices.size() == 0) {
            prompt_user("小扳手尚未连接至手机，请检查手机adb是否打开、驱动是否安装，USB线是否已连接");
        }

        QString adb_current_device = AdbClient::getAdbSerial();
        if (adb_devices.contains(adb_current_device)) {
            // do nothing
        } else {
            AdbClient::setAdbSerial(
                selectArgs("你想连接到哪个 adb 设备？", adb_devices)
                );
        }

        return;
    }

    if (!mNotificationOnline) {
        prompt_user("小扳手已连接至手机，但无法接收手机端的通知。需要打开手机第三方通知设置，并允许小扳手辅助App接收通知");
        mLuaThread->addScript(QStringList() << "call_ext" << "switch-notification");
        return;
    }

    onShowNotifications();
}

void WrenchMainWindow::on_tbWeixin_pressed()
{
    weixinSelectContactTimer.start(500);
}

void WrenchMainWindow::on_tbQq_pressed()
{
    qqSelectContactTimer.start(500);
}

void WrenchMainWindow::on_tbWeibo_pressed()
{
    weiboSelectContactTimer.start(500);
}

void WrenchMainWindow::on_tbWeibo_released()
{
    weiboSelectContactTimer.stop();
}

void WrenchMainWindow::on_tbWeixin_released()
{
    weixinSelectContactTimer.stop();

}

void WrenchMainWindow::on_tbQq_released()
{
    qqSelectContactTimer.stop();
}

void WrenchMainWindow::selectQqContact()
{
    mLuaThread->addScript(QStringList() << "wrench_call" << "@@qq");
    ui->tbQq->click();
    ui->tbQq->click();
}

void WrenchMainWindow::selectWeixinContact()
{
    mLuaThread->addScript(QStringList() << "wrench_call" << "@@wx");
    ui->tbWeixin->click();
    ui->tbWeixin->click();
}

void WrenchMainWindow::selectWeiboContact()
{
    mLuaThread->addScript(QStringList() << "wrench_call" << "@@wb");
    ui->tbWeibo->click();
    ui->tbWeibo->click();
}

void WrenchMainWindow::changeEvent(QEvent *event)
{
    if (event->type() == QEvent::ActivationChange &&
        vncMainWindow &&
        ! vncMainWindow->isHidden() &&
        ui->tbPhoneScreen->isChecked()) {
        QTimer::singleShot(100, [&]() {
                if (this->isActiveWindow()) {
                    vncMainWindow->raise();
                }
            });
    }
}
