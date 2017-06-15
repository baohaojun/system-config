/* -*- mode: c++ -*- */
#ifndef WRENCHMAINWINDOW_H
#define WRENCHMAINWINDOW_H

#include <QtWidgets/QMainWindow>
#include <QtWidgets/QRadioButton>
#include "luaexecutethread.hpp"
//#include "screencapture.h"
#include "wrenchext.h"
#include <QtCore/QSettings>
#include "dialoggetentry.h"
#include <QMenu>
#include <QSystemTrayIcon>
#include <QAction>
#include <QCloseEvent>
#include <QDragEnterEvent>
#include <QDropEvent>
#include "contactmodel.h"
#include <QMap>
#include "phonescreendialog.h"
#include <QShowEvent>
#include "vncmainwindow.h"
#include <QMoveEvent>
#include <QMutex>
#include <QHash>
#include <libsnore/application.h>
#include <libsnore/snore.h>
#include <libsnore/notification/icon.h>
#include <QHotkey>
#include <QNetworkAccessManager>
#include <QTime>
#include <QTimer>

namespace Ui {
class WrenchMainWindow;
}

class WrenchMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit WrenchMainWindow(QWidget *parent = 0);
    ~WrenchMainWindow();
    void dragEnterEvent(QDragEnterEvent *event);
    void dropEvent(QDropEvent *event);
    void changeEvent(QEvent *event);

public slots:
    void handleNetworkData(QNetworkReply*);
    void adbStateUpdated(const QString& state);
    void onInfoUpdate(const QString& key, const QString& val);
    void onSelectArgs(const QStringList& args);
    void onSelectApps();
    void onShowNotifications();
    void startTask(const QString& task);
    void on_argSelected(const QString& arg);
    void on_appSelected(const QString& app);
    void moveVncMainWin();
    void moveVncMainWinWhenMoving();
    void moveEvent(QMoveEvent*);
private:
    void movePhoneScreenWindowXY(int x, int y);
    bool mLastNotificationIsSelection;
    QMap<QString, QString> mLastClickedNotification;
    void saveLastClickedSnoreNotification(uint);
    void addLog(const QString&);
private slots:
    void selectQqContact();
    void selectWeixinContact();
    void imageDropped(const QDropEvent& ev);
    void iconActivated(QSystemTrayIcon::ActivationReason reason);
    void slotHandleCaptureScreen(const QPixmap &);
    void onAdbNotificationArrived(const QString& , const QString& , const QString& , const QString&, const QString&);
    void onLoadMailHeads(const QString& , const QString& , const QString& , const QString& , const QString&);
    void on_sendItPushButton_clicked();

    void on_configurePushButton_clicked();

    void on_tbScreenCapture_clicked();

    void on_tbPicture_clicked();

    void on_tbEmoji_clicked();

    void on_tbWeibo_clicked();

    void on_tbWeixin_clicked();

    void on_tbQq_clicked();

    void on_tbMomo_clicked();

    void on_tbThumbsUp_clicked();

    void on_tbPhoneCall_clicked();
    void on_tbMms_clicked();

    void on_tbNotes_clicked();

    void on_Dial(const QString&);
    void on_MailTo(const QString&);
    void on_MailCc(const QString&);
    void on_MailBcc(const QString&);
    void on_addMmsReceiver(const QString&, const QString&);
    void quitMyself();

    void on_tbMailAddTo_clicked();

    void on_tbMailAddCc_clicked();

    void on_tbMailAddBcc_clicked();

    void on_tbMailAddAttachment_clicked();

    void on_tbMailDone_clicked();

    void on_tbMailLoad_clicked();

    void on_tbMailSave_clicked();

    void on_tbMailClear_clicked();

    void on_tbPhoneScreen_toggled(bool checked);

    void on_tbLauncher_clicked();
    void slotNotificationClosed(Snore::Notification);
    void slotShortCutActivated();
    void adbNotificationShiftClicked(const QMap<QString, QString>& rawData);
    void saveLastDialogClickedNotification(const QMap<QString, QString>&);

    void on_adbStateIndicator_clicked();

    void on_tbWeixin_pressed();

    void on_tbQq_pressed();

    void on_tbWeixin_released();

    void on_tbQq_released();

private:


    QTimer weixinSelectContactTimer;
    QTimer qqSelectContactTimer;
    bool mNotificationOnline;
    bool mInputOnline;
    void deleteLuaThread();
    void sharePictures(const QStringList& files);
    QNetworkAccessManager m_manager;
    uint m_last_shown_notification_id;
    uint m_last_closed_notification_id;
    Snore::Notification m_last_notification;
    QHotkey m_hotkey;
    Snore::SnoreCore *m_snore;
    Snore::Application m_snore_application;
    Snore::Icon m_defaultIcon;
    QHash<uint, QString> m_notification_map;
    Snore::Alert m_alert;
    QHash<QString, Snore::Icon> m_pkg_icons;

    Snore::Icon getPkgIcon(const QString& pkg);

    WrenchExt mWrenchExt;

    bool anyShareChecked();

    QMap<QString, QString> mMmsReceiverMap;

    DialogGetEntry* mSelectArgDialog;
    void createTrayIcon();
    void closeEvent(QCloseEvent *event);
    void showEvent(QShowEvent *event);

    QAction* quitAction;
    QAction* showNotificationAction;
    QSystemTrayIcon* trayIcon;
    QMenu* trayIconMenu;
    bool mQuit;
    //QSharedPointer<ScreenCapture> mScreenCapture;
    QSharedPointer<LuaExecuteThread> mLuaThread;
    QSharedPointer<DialogGetEntry> mEmojiDialog;
    QSharedPointer<DialogGetEntry> mContactDialog;
    QSharedPointer<PhoneScreenDialog> mPhoneScreenDialog;

    void afterUsingContactDialog();
    ContactModel* mContactModel;
    void initContactDialog(bool isMail = false);
    Ui::WrenchMainWindow *ui;
    QSettings mSettings;
    QStringList mPictures;
    QString get_text();
    void getclip_android();
    QRadioButton* mLastRadioButton;
    virtual bool eventFilter(QObject *obj, QEvent *ev);
    bool handleEmacsKeys(QWidget *w, QKeyEvent *e);
    friend class PhoneScreenDialog;
    friend class VncMainWindow;


signals:
    void activateWindow();
    void adbNotificationClicked(const QString& key);
};

#endif // WRENCHMAINWINDOW_H
