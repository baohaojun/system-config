#ifndef MYFORM_H
#define MYFORM_H

#include <QWidget>

namespace Ui {
class MyForm;
}

class MyForm : public QWidget
{
    Q_OBJECT

public:
    explicit MyForm(QWidget *parent = 0);
    ~MyForm();

private:
    Ui::MyForm *ui;
};

#endif // MYFORM_H
