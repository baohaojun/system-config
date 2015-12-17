
#ifndef SOUNDSETTINGS_H
#define SOUNDSETTINGS_H

#include "libsnore/settings/pluginsettingswidget.h"
#include "libsnore/plugins/settingsplugin.h"

class QLineEdit;
class QSpinBox;
class SoundSettings : public Snore::PluginSettingsWidget
{
    Q_OBJECT

public:
    explicit SoundSettings ( Snore::SnorePlugin* snorePlugin, QWidget* parent = nullptr );
    ~SoundSettings();

    void load() override;
    void save() override;

private:
    QLineEdit* m_lineEditFileName;
    QSpinBox* m_spinBoxVolume;

};

SNORE_DECLARE_SETTINGS_PLUGIN ( SoundSettings )

#endif // SOUNDSETTINGS_H
