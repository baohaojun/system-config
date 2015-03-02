#ifndef SETTINGSWINDOW_H
#define SETTINGSWINDOW_H

#include <QMainWindow>

namespace Ui {
class SettingsWindow;
}

class SettingsWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit SettingsWindow(QWidget *parent = 0);
    ~SettingsWindow();

private slots:
    void on_comboBox_currentIndexChanged(const QString &arg1);

private:
    Ui::SettingsWindow *ui;
};

#endif // SETTINGSWINDOW_H
