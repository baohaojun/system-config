#include "phonescreen.h"
#include "ui_phonescreen.h"

PhoneScreen::PhoneScreen(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::PhoneScreen)
{
    ui->setupUi(this);
}

PhoneScreen::~PhoneScreen()
{
    delete ui;
}
