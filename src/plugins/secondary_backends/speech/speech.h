#ifndef SPEECH_H
#define SPEECH_H

#include <plugins/snoresecondarybackend.h>

class QTextToSpeech;

class Speech : public Snore::SnoreSecondaryBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreSecondaryBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.SecondaryNotificationBackend/1.0" FILE "snore_plugin.json")
public:
    explicit Speech();
    ~Speech() = default;

public Q_SLOTS:
    void slotNotificationDisplayed(Snore::Notification notification) override;
private:
    QTextToSpeech *m_speech;
};

#endif // SPEECH_H
