#ifndef T1WRENCHMAINWINDOW_H
#define T1WRENCHMAINWINDOW_H

#include <QtWidgets/QMainWindow>
#include <QtWidgets/QRadioButton>
#include "luaexecutethread.hpp"
#include "screencapture.h"
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

namespace Ui {
class T1WrenchMainWindow;
}

class T1WrenchMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit T1WrenchMainWindow(QWidget *parent = 0);
    ~T1WrenchMainWindow();
    void dragEnterEvent(QDragEnterEvent *event);
    void dropEvent(QDropEvent *event);

public slots:
    void adbStateUpdated(const QString& state);
    void onInfoUpdate(const QString& key, const QString& val);
    void onSelectArgs(const QStringList& args);
    void startTask(const QString& task);
    void on_argSelected(const QString& arg);
private slots:
    void iconActivated(QSystemTrayIcon::ActivationReason reason);
    void slotHandleCaptureScreen(const QPixmap &);
    void on_sendItPushButton_clicked();

    void on_configurePushButton_clicked();

    void on_tbScreenCapture_clicked();

    void on_tbPicture_clicked();

    void on_tbEmoji_clicked();

    void on_tbWeibo_clicked();

    void on_tbWeixin_clicked();

    void on_tbMomo_clicked();

    void on_tbThumbsUp_clicked();

    void on_tbPhoneCall_clicked();
    void on_tbMms_clicked();

    void on_tbNotes_clicked();

    void on_Dial(const QString&);
    void on_MailTo(const QString&);
    void on_addMmsReceiver(const QString&, const QString&);
    void quitMyself();

    void on_tbMailAddTo_clicked();

private:

    QMap<QString, QString> mMmsReceiverMap;

    DialogGetEntry* mSelectArgDialog;
    void createTrayIcon();
    void closeEvent(QCloseEvent *event);

    QAction* quitAction;
    QSystemTrayIcon* trayIcon;
    QMenu* trayIconMenu;
    bool mQuit;
    QSharedPointer<ScreenCapture> mScreenCapture;
    QSharedPointer<LuaExecuteThread> mLuaThread;
    QSharedPointer<DialogGetEntry> mEmojiDialog;
    QSharedPointer<DialogGetEntry> mContactDialog;

    void afterUsingContactDialog();
    ContactModel* mContactModel;
    void initContactDialog(bool isMail = false);
    Ui::T1WrenchMainWindow *ui;
    QSettings mSettings;
    QStringList mPictures;
    QString get_text();
    void getclip_android();
    QRadioButton* mLastRadioButton;
signals:
    void activateWindow();
};

#endif // T1WRENCHMAINWINDOW_H
