#ifndef SETTINGSDIALOG_H
#define SETTINGSDIALOG_H

#include <QDialog>

namespace Snore {
class SnoreCore;
class PluginSettingsWidget;
}

namespace Ui {
class SettingsDialog;
}

class SettingsDialog : public QDialog
{
    Q_OBJECT

public:
    explicit SettingsDialog(Snore::SnoreCore *snore,QWidget *parent = 0);
    ~SettingsDialog();

private slots:
    void on_buttonBox_accepted();

private:
    Ui::SettingsDialog *ui;
    Snore::SnoreCore *m_snore;
    QList<Snore::PluginSettingsWidget*> m_tabs;
};

#endif // SETTINGSDIALOG_H
