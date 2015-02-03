
#ifndef SOUNDSETTINGS_H
#define SOUNDSETTINGS_H

#include "plugins/pluginsettingswidget.h"

class QLineEdit;
class SoundSettings : public Snore::PluginSettingsWidget
{
    Q_OBJECT

public:
    explicit SoundSettings(Snore::SnorePlugin *snorePlugin, QWidget *parent = nullptr);
    ~SoundSettings();

    void load() override;
    void save() override;

private:
    QLineEdit *m_lineEdit;

};

#endif // SOUNDSETTINGS_H
