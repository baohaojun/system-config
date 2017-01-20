#include "speech.h"

#include <QtTextToSpeech/QTextToSpeech>
Speech::Speech()
    : Snore::SnoreSecondaryBackend()
    , m_speech(new QTextToSpeech(this))
{
    connect(m_speech, &QTextToSpeech::stateChanged, this, [this](QTextToSpeech::State state){
        qCDebug(SNORE) << state;
        if (state == QTextToSpeech::BackendError) {
            setErrorString(tr("Sytem Backend Error"));
        }
    });
}

void Speech::slotNotificationDisplayed(Snore::Notification notification)
{
    m_speech->say(tr("%1: %2 %3").arg(notification.application().name(), notification.title(), notification.text()));
}
