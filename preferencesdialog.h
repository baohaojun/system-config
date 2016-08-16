#ifndef PREFERENCESDIALOG_H
#define PREFERENCESDIALOG_H

#include <QDialog>
#include <QStringList>

namespace Ui {
class PreferencesDialog;
}

class PreferencesDialog : public QDialog
{
    Q_OBJECT

public:
    explicit PreferencesDialog(QWidget *parent = 0);
    ~PreferencesDialog();

signals:

public slots:
    void restoreSettings();
    void applySettings();
    void on_pushButtonOk_clicked();
    void on_pushButtonApply_clicked();
    void on_pushButtonCancel_clicked();

protected:
    void showEvent(QShowEvent *e);

private:
    Ui::PreferencesDialog *ui;
    QStringList m_renderers;
};

#endif // PREFERENCESDIALOG_H
