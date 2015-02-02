
#ifndef SOUNDSETTINGS_H
#define SOUNDSETTINGS_H

#include "plugins/pluginsettingswidget.h"


namespace Ui {
class SoundSettings;
}

class SoundSettings : public Snore::PluginSettingsWidget
{
    Q_OBJECT

public:
    explicit SoundSettings(Snore::SnorePlugin *snorePlugin, QWidget *parent = nullptr);
    ~SoundSettings();

    void load() override;
    void save() override;

private slots:
    void on_pushButton_clicked();

private:
    Ui::SoundSettings *ui;

};

#endif // SOUNDSETTINGS_H
