#include "rangeslider.h"

RangeSlider::RangeSlider(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::RangeSlider)
{
    ui->setupUi(this);
}

RangeSlider::~RangeSlider()
{
    delete ui;
}
