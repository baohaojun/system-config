#include <QtGui>
#include <QApplication>
#include <QFileDialog>
#include <QMessageBox>
#include <QStyleFactory>
#include <QString>
#include <QFont>

#include "macros.h"
#include "preferencesdialog.h"
#include "ui_preferencesdialog.h"
#include "connectionwindow.h"
#include "qvncviewersettings.h"
#include "mainwindow.h"

extern QtVncViewerSettings *globalConfig;
extern MainWindow *mainWindow;

PreferencesDialog::PreferencesDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::PreferencesDialog)
{
    ui->setupUi(this);
    setModal(true);
    ui->comboBoxStyle->addItems(QStyleFactory::keys());
    m_renderers << tr("Raster") << tr("OpenGL");
    ui->comboBoxRenderer->addItems(m_renderers);
    ui->comboBoxEncoding->addItems(MainWindow::encodings());
    restoreSettings();
}

PreferencesDialog::~PreferencesDialog()
{
    delete ui;
}

void PreferencesDialog::restoreSettings()
{
    // GUI settings
    QString lang = globalConfig->preferencesLanguage();
    int i = ui->comboBoxLanguage->findText("(" + lang + ")", Qt::MatchEndsWith);
    if ( i >= 0 )
        ui->comboBoxLanguage->setCurrentIndex(i);
    else
        ui->comboBoxLanguage->setCurrentIndex(ui->comboBoxLanguage->findText("(us)", Qt::MatchEndsWith));
    i = ui->comboBoxStyle->findText(globalConfig->preferencesGuiStyle());
    if ( i >= 0 )
        ui->comboBoxStyle->setCurrentIndex(i);
    QFont f;
    if ( !f.fromString(globalConfig->preferencesAppFont()) )
        f = qApp->font();
    ui->fontComboBoxAppFont->setCurrentFont(f);
    ui->spinBoxAppFontSize->setValue(globalConfig->preferencesAppFontSize());
    ui->checkBoxMaximizeWindows->setChecked(globalConfig->preferencesMaximizeWindows());
    ui->checkBoxNativeFileDialogs->setChecked(globalConfig->preferencesNativeFileDialogs());
    ui->checkBoxQuiet->setChecked(globalConfig->preferencesQuiet());

    // VNC defaults
    ui->checkBoxScaledDisplay->setChecked(globalConfig->preferencesScaled());
    ui->checkBoxBilinearFiltering->setChecked(globalConfig->preferencesBilinearFilter());
    ui->checkBoxKeepAspect->setChecked(globalConfig->preferencesKeepAspect());
    ui->checkBoxShowFps->setChecked(globalConfig->preferencesShowFps());
    if ( globalConfig->preferencesSurfaceType() > m_renderers.count() )
        ui->comboBoxRenderer->setCurrentIndex(QVNCVIEWER_SURFACE_RASTER);
    else
        ui->comboBoxRenderer->setCurrentIndex(globalConfig->preferencesSurfaceType());
    if ( !mainWindow->encodings().contains(globalConfig->preferencesEncoding()) )
        globalConfig->setPreferencesEncoding("Hextile");
    ui->comboBoxEncoding->setCurrentIndex(ui->comboBoxEncoding->findText(globalConfig->preferencesEncoding()));
    ui->sliderQuality->setValue(globalConfig->preferencesQualityLevel());
    ui->sliderCompression->setValue(globalConfig->preferencesCompressionLevel());
}

void PreferencesDialog::applySettings()
{
    // GUI settings
    QString lang = ui->comboBoxLanguage->currentText();
    globalConfig->setPreferencesLanguage(lang.mid(lang.indexOf("(") + 1, 2));
    globalConfig->setPreferencesGuiStyle(ui->comboBoxStyle->currentText());
    qApp->setStyle(globalConfig->preferencesGuiStyle());
    globalConfig->setPreferencesAppFont(ui->fontComboBoxAppFont->currentFont().toString());
    globalConfig->setPreferencesAppFontSize(ui->spinBoxAppFontSize->value());
    QFont f;
    f.fromString(globalConfig->preferencesAppFont());
    f.setPointSize(globalConfig->preferencesAppFontSize());
    qApp->setFont(f);
    globalConfig->setPreferencesMaximizeWindows(ui->checkBoxMaximizeWindows->isChecked());
    if ( globalConfig->preferencesMaximizeWindows() ) {
        foreach (QMdiSubWindow *w, mainWindow->mdiArea()->subWindowList())
            w->showMaximized();
    }
    globalConfig->setPreferencesNativeFileDialogs(ui->checkBoxNativeFileDialogs->isChecked());
    globalConfig->setPreferencesQuiet(ui->checkBoxQuiet->isChecked());
    ConnectionWindow::setQuiet(globalConfig->preferencesQuiet());

    // VNC defaults
    globalConfig->setPreferencesScaled(ui->checkBoxScaledDisplay->isChecked());
    globalConfig->setPreferencesBilinearFilter(ui->checkBoxBilinearFiltering->isChecked());
    globalConfig->setPreferencesKeepAspect(ui->checkBoxKeepAspect->isChecked());
    globalConfig->setPreferencesShowFps(ui->checkBoxShowFps->isChecked());
    globalConfig->setPreferencesSurfaceType(ui->comboBoxRenderer->currentIndex());
    globalConfig->setPreferencesEncoding(ui->comboBoxEncoding->currentText());
    globalConfig->setPreferencesQualityLevel(ui->sliderQuality->value());
    globalConfig->setPreferencesCompressionLevel(ui->sliderCompression->value());
}

void PreferencesDialog::on_pushButtonOk_clicked()
{
    applySettings();
}

void PreferencesDialog::on_pushButtonApply_clicked()
{
    applySettings();
    adjustSize();
}

void PreferencesDialog::on_pushButtonCancel_clicked()
{
    restoreSettings();
}

void PreferencesDialog::showEvent(QShowEvent *e)
{
    adjustSize();
    QDialog::showEvent(e);
}
