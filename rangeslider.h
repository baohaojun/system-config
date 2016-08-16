#ifndef RANGESLIDER_H
#define RANGESLIDER_H

#include <QWidget>
#include <QSlider>
#include <QLabel>
#include <QLCDNumber>

#include "ui_rangeslider.h"

namespace Ui {
class RangeSlider;
}

class RangeSlider : public QWidget
{
    Q_OBJECT

public:
    explicit RangeSlider(QWidget *parent = 0);
    ~RangeSlider();

    QSlider *slider() { return ui->slider; }
    QLabel *label() { return ui->label; }
    QLCDNumber *lcd() { return ui->lcd; }

private:
    Ui::RangeSlider *ui;
};

#endif // RANGESLIDER_H
