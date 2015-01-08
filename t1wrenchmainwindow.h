#ifndef T1WRENCHMAINWINDOW_H
#define T1WRENCHMAINWINDOW_H

#include <QtWidgets/QMainWindow>
#include <QtWidgets/QRadioButton>
#include "luaexecutethread.hpp"
#include "screencapture.h"
#include <QtCore/QSettings>
#include "dialoggetemoji.h"
#include "dialoggetcontact.h"
#include <QMenu>
#include <QSystemTrayIcon>
#include <QAction>
#include <QCloseEvent>

namespace Ui {
class T1WrenchMainWindow;
}

class T1WrenchMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit T1WrenchMainWindow(QWidget *parent = 0);
    ~T1WrenchMainWindow();
public slots:
    void adbStateUpdated(const QString& state);
    void onInfoUpdate(const QString& key, const QString& val);
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

    void on_tbNotes_clicked();

    void on_contactSelected(const QString&);
    void quitMyself();

private:
    void createTrayIcon();
    void closeEvent(QCloseEvent *event);

    QAction* quitAction;
    QSystemTrayIcon* trayIcon;
    QMenu* trayIconMenu;
    bool mQuit;
    QSharedPointer<ScreenCapture> mScreenCapture;
    QSharedPointer<LuaExecuteThread> mLuaThread;
    QSharedPointer<DialogGetEmoji> mEmojiDialog;
    QSharedPointer<DialogGetContact> mContactDialog;
    Ui::T1WrenchMainWindow *ui;
    QSettings mSettings;
    QStringList mPictures;
    QString get_text();
    void getclip_android();
    QRadioButton* mLastRadioButton;
};

#endif // T1WRENCHMAINWINDOW_H
