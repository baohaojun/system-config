#ifndef SETTINGSWINDOW_H
#define SETTINGSWINDOW_H

#include <QMainWindow>
#include <QSettings>

namespace Ui
{
class SettingsWindow;
}

class QAbstractButton;

class SettingsWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit SettingsWindow(QWidget *parent = 0);
    ~SettingsWindow();

    static QStringList knownApps();

    static QSettings &settings();

    template<typename Func>
    static QStringList allSettingsKeysWithPrefix(const QString &prefix, QSettings &settings, Func fun)
    {
        QStringList groups = prefix.split(QLatin1Char('/'));
        QStringList out;

        for (const QString group : groups) {
            settings.beginGroup(group);
        }
        out = fun(settings);

        for (int i = 0; i < groups.size(); ++i) {
            settings.endGroup();
        }
        return out;
    }

private Q_SLOTS:
    void on_buttonBox_clicked(QAbstractButton *button);
    void on_comboBox_currentIndexChanged(const QString &arg1);

private:
    Ui::SettingsWindow *ui;
};

#endif // SETTINGSWINDOW_H
