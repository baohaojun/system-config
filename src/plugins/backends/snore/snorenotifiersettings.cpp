#include "snorenotifiersettings.h"
#include "ui_snorenotifiersettings.h"
#include "snore.h"

using namespace Snore;

SnoreNotifierSettings::SnoreNotifierSettings(SnorePlugin *snore, QWidget *parent) :
    PluginSettingsWidget(snore,parent),
    ui(new Ui::SnoreNotifierSettings)
{
    ui->setupUi(this);
    ui->comboBox->addItem("TopLeftCorner",Qt::TopLeftCorner);
    ui->comboBox->addItem("TopRightCorner",Qt::TopRightCorner);
    ui->comboBox->addItem("BottomLeftCorner",Qt::BottomLeftCorner);
    ui->comboBox->addItem("BottomRightCorner",Qt::BottomRightCorner);
}

SnoreNotifierSettings::~SnoreNotifierSettings()
{
    delete ui;
}

void SnoreNotifierSettings::load()
{
    ui->comboBox->setCurrentIndex(m_snorePlugin->value("Position").toInt());
}

void SnoreNotifierSettings::save()
{
    m_snorePlugin->setValue("Position", ui->comboBox->currentIndex());
}
