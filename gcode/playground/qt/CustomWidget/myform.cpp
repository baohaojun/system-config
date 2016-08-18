#include "myform.h"
#include "ui_myform.h"

MyForm::MyForm(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::MyForm)
{
    ui->setupUi(this);
}

MyForm::~MyForm()
{
    delete ui;
}
