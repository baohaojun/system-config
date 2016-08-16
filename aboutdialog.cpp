#include <QString>

#include "aboutdialog.h"
#include "ui_aboutdialog.h"
#include "macros.h"

AboutDialog::AboutDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::AboutDialog)
{
    ui->setupUi(this);
    ui->labelInfo->setText(QString("<center><p><font size=\"+2\"><b>" +
                                   tr("Qt VNC Viewer") + " " + QVNCVIEWER_APP_VERSION + "</b></font></p><p>" +
                                   tr("Qt based VNC client with MAME/MESS/UME (VNC OSD) specific extensions") + "</p><p>" +
                                   tr("Copyright") + " &copy; 2015 Ren&eacute; Reucher</p></center>"));
    adjustSize();
    setFixedSize(size());
}

AboutDialog::~AboutDialog()
{
    delete ui;
}
