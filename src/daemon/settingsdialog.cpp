#include "settingsdialog.h"
#include "ui_settingsdialog.h"
#include <QTabWidget>

#include "snore.h"

using namespace Snore;

SettingsDialog::SettingsDialog(SnoreCore *snore, QWidget *parent) :
    QDialog(parent),
    ui(new Ui::SettingsDialog),
    m_snore(snore)
{
    ui->setupUi(this);
    ui->tabWidget->clear();
    for(auto widget : snore->settingWidgets())
    {
        ui->tabWidget->addTab(widget,widget->name());
        widget->load();
        m_tabs.append(widget);
    }
}

SettingsDialog::~SettingsDialog()
{
    delete ui;
}

void SettingsDialog::on_buttonBox_accepted()
{
    for( auto w:m_tabs)
    {
        w->save();
    }
}
