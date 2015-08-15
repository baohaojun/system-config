#ifndef PHONESCREEN_H
#define PHONESCREEN_H

#include <QDialog>

namespace Ui {
class PhoneScreen;
}

class PhoneScreen : public QDialog
{
    Q_OBJECT

public:
    explicit PhoneScreen(QWidget *parent = 0);
    ~PhoneScreen();

private:
    Ui::PhoneScreen *ui;
};

#endif // PHONESCREEN_H
