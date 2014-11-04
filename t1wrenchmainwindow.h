#ifndef T1WRENCHMAINWINDOW_H
#define T1WRENCHMAINWINDOW_H

#include <QtWidgets/QMainWindow>
#include <QtWidgets/QRadioButton>
#include "luaexecutethread.hpp"
#include "screencapture.h"

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
    void slotHandleCaptureScreen(const QPixmap &);
    void on_weixinQqRadio_toggled(bool checked);

    void on_replyMailRadio_toggled(bool checked);

    void on_replySmsRadio_toggled(bool checked);

    void on_weiboRadio_toggled(bool checked);

    void on_googlePlusRadio_toggled(bool checked);

    void on_toClipBoardRadio_toggled(bool checked);

    void on_fromClipBoard_toggled(bool checked);

    void on_sendItPushButton_clicked();

    void on_configurePushButton_clicked();

    void on_tbScreenCapture_clicked();

private:
    QSharedPointer<ScreenCapture> mScreenCapture;
    LuaExecuteThread* mLuaThread;
    Ui::T1WrenchMainWindow *ui;
    QString get_text();
    void getclip_android();
    QRadioButton* mLastRadioButton;
};

#endif // T1WRENCHMAINWINDOW_H
