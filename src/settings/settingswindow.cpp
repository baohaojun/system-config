#include "settingswindow.h"
#include "ui_settingswindow.h"

#include "snore.h"
#include "snore_p.h"
#include "settingsdialog.h"

#include <QApplication>
#include <QComboBox>

using namespace Snore;

SettingsWindow::SettingsWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::SettingsWindow)
{
    ui->setupUi(this);
    ui->widget->show();
    connect(ui->widget, &Snore::SettingsDialog::finished, qApp, &QApplication::quit);

    QStringList list = SnoreCorePrivate::instance()->knownClients();
    list.removeAll(QString("%1.%2").arg(qApp->organizationName(), qApp->applicationName()));
    ui->comboBox->addItems(list);
}

SettingsWindow::~SettingsWindow()
{
    delete ui;
}

void SettingsWindow::on_comboBox_currentIndexChanged(const QString &arg1)
{
    SnoreCorePrivate::instance()->setLocalSttingsPrefix(arg1);
    ui->widget->show();
}
