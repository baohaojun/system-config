#ifndef T1WRENCHMAINWINDOW_H
#define T1WRENCHMAINWINDOW_H

#include <QtWidgets/QMainWindow>

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
    void on_qqButton_clicked();

    void on_cellMailButton_clicked();

    void on_t1SmsButton_clicked();

    void on_weiboButton_clicked();

    void on_googlePlusButton_clicked();

    void on_toPhoneClipboardButton_clicked();

    void on_fromPhoneClipboardButton_clicked();

    void on_qqButton_pressed();

    void on_cellMailButton_pressed();

    void on_t1SmsButton_pressed();

    void on_weiboButton_pressed();

    void on_googlePlusButton_pressed();

    void on_toPhoneClipboardButton_pressed();

    void on_fromPhoneClipboardButton_pressed();

private:
    Ui::T1WrenchMainWindow *ui;
    void putclip_android();
};

#endif // T1WRENCHMAINWINDOW_H
