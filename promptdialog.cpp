#include "promptdialog.h"
#include "ui_promptdialog.h"

PromptDialog::PromptDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::PromptDialog)
{
    ui->setupUi(this);
}

PromptDialog::~PromptDialog()
{
    delete ui;
}
