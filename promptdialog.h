#ifndef PROMPTDIALOG_H
#define PROMPTDIALOG_H

#include <QDialog>

namespace Ui {
class PromptDialog;
}

class PromptDialog : public QDialog
{
    Q_OBJECT

public:
    explicit PromptDialog(QWidget *parent = 0);
    ~PromptDialog();

private:
    Ui::PromptDialog *ui;
};

#endif // PROMPTDIALOG_H
