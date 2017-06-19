/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2015  Hannah von Reth <vonreth@kde.org>

    SnoreNotify is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    SnoreNotify is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with SnoreNotify.  If not, see <http://www.gnu.org/licenses/>.
*/
#include "pushoversettings.h"
#include "pushoverclient.h"

#include "libsnore/plugins/plugins.h"
#include "libsnore/hint.h"

#include <QLineEdit>
#include <QPushButton>
#include <QLabel>

PushoverSettings::PushoverSettings(Snore::SnorePlugin *plugin, QWidget *parent) :
    Snore::PluginSettingsWidget(plugin, parent),
    m_emailLineEdit(new QLineEdit(this)),
    m_passwordLineEdit(new QLineEdit(this)),
    m_deviceLineEdit(new QLineEdit(this)),
    m_registerButton(new QPushButton(this)),
    m_errorMessageLabel(new QLabel(this))
{
    m_passwordLineEdit->setEchoMode(QLineEdit::Password);
    addRow(tr("Email Address:"), m_emailLineEdit);
    addRow(tr("Password:"), m_passwordLineEdit);
    addRow(tr("Device Name:"), m_deviceLineEdit);
    addRow(QString(), m_registerButton);
    addRow(tr("Status"), m_errorMessageLabel);
    addRow(QString(), new QLabel(this));
    addRow(QString(), new QLabel(tr("If you don't have an account yet please register at <a href=\"https://pushover.net\">Pushover.net</a>"), this));

    m_emailLineEdit->setEnabled(false);
    m_passwordLineEdit->setEnabled(false);
    m_deviceLineEdit->setEnabled(false);
    m_registerButton->setEnabled(false);

    QPointer<PushoverClient> pushover = plugin->constHints().value("client").value<QPointer<PushoverClient>>();
    Q_ASSERT_X(pushover, Q_FUNC_INFO, "Failed to retrieve PushoverClient.");

    if (pushover) {

        m_errorMessageLabel->setText(pushover->errorMessage());

        connect(pushover, &PushoverClient::loggedInChanged, this, &PushoverSettings::slotUpdateLoginState);
        connect(pushover, &PushoverClient::error, [this](const QString & message) {
            m_errorMessageLabel->setText(message);
        });

        slotUpdateLoginState(pushover->isLoggedIn());

        connect(m_registerButton, &QPushButton::clicked, [pushover, this]() {
            m_registerButton->setEnabled(false);
            if (pushover->isLoggedIn() != PushoverClient::LoggedIn) {
                pushover->login(m_emailLineEdit->text(), m_passwordLineEdit->text(), m_deviceLineEdit->text());
            } else {
                pushover->logOut();
            }
        });

    }

}

PushoverSettings::~PushoverSettings()
{
}

void PushoverSettings::load()
{
    m_deviceLineEdit->setText(settingsValue(QStringLiteral("DeviceName"), Snore::LocalSetting).toString());
}

void PushoverSettings::save()
{
}

void PushoverSettings::slotUpdateLoginState(PushoverClient::LoginState state)
{
    if (state != m_state) {
        m_state = state;
        if (state == PushoverClient::LoggedIn) {
            m_emailLineEdit->setEnabled(false);
            m_passwordLineEdit->setEnabled(false);
            m_deviceLineEdit->setEnabled(false);
            m_registerButton->setText(tr("Log out"));
        } else {
            m_emailLineEdit->setEnabled(true);
            m_passwordLineEdit->setEnabled(true);
            m_deviceLineEdit->setEnabled(true);
            m_registerButton->setText(tr("Log in"));
        }
        switch (state) {
        case PushoverClient::LoggedIn:
            m_errorMessageLabel->setText(tr("Logged in."));
            break;
        case PushoverClient::LoggedOut:
            m_errorMessageLabel->setText(tr("Logged out."));
            break;
        default:
            break;
        }
    }
    m_registerButton->setEnabled(true);
}
