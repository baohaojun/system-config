#include "speech.h"

#include <QtTextToSpeech/QTextToSpeech>

#include <snore.h>
Speech::Speech()
    : Snore::SnoreSecondaryBackend()
    , m_speech(new QTextToSpeech(this))
{
    connect(m_speech, &QTextToSpeech::stateChanged, this, [this](QTextToSpeech::State state) {
        qCDebug(SNORE) << state;
        if (state == QTextToSpeech::BackendError) {
            setErrorString(tr("System Backend Error"));
        }
    });
    connect(this, &Speech::enabledChanged, this, [this](bool b) {
        if (b) {
            connect(&Snore::SnoreCore::instance(), &Snore::SnoreCore::notificationClosed, this, &Speech::slotNotificationClosed);
        } else {
            disconnect(&Snore::SnoreCore::instance(), &Snore::SnoreCore::notificationClosed, this, &Speech::slotNotificationClosed);
        }
    });
}

void Speech::slotNotificationDisplayed(Snore::Notification notification)
{
    m_speech->say(tr("%1: %2 %3").arg(notification.application().name(), notification.title(), notification.text()));
}

void Speech::slotNotificationClosed(Snore::Notification notificatiom) {
    // TODO: que notifications on our side and make sure to only stop playback of the canceled notification
    if (notificatiom.closeReason() != Snore::Notification::TimedOut && m_speech->state() == QTextToSpeech::Speaking) {
        m_speech->stop();
    }
}
