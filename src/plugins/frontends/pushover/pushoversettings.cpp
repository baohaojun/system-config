/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2015  Patrick von Reth <vonreth@kde.org>

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
#include "pushover_frontend.h"

#include "plugins/plugins.h"

#include <QLineEdit>
#include <QPushButton>
#include <QTimer>

PushoverSettings::PushoverSettings(Snore::SnorePlugin *plugin, QWidget *parent) :
    Snore::PluginSettingsWidget(plugin, parent),
    m_emailLineEdit(new QLineEdit(this)),
    m_passwordLineEdit(new QLineEdit(this)),
    m_deviceLineEdit(new QLineEdit(this)),
    m_registerButton(new QPushButton(this))
{
    m_passwordLineEdit->setEchoMode(QLineEdit::Password);
    addRow(tr("Email Address:"), m_emailLineEdit);
    addRow(tr("Password:"), m_passwordLineEdit);
    addRow(tr("Device Name:"), m_deviceLineEdit);
    updateLoginState();
    addRow(QString(), m_registerButton);

    PushoverFrontend *pushover = dynamic_cast<PushoverFrontend*>(plugin);
    connect(m_registerButton, &QPushButton::clicked, [pushover, this] () {
        if(!value(QLatin1String("Registered"), Snore::LOCAL_SETTING).toBool()) {
            pushover->registerDevice(m_emailLineEdit->text(), m_passwordLineEdit->text(), m_deviceLineEdit->text());
            setValue(QLatin1String("DeviceName"), m_deviceLineEdit->text(), Snore::LOCAL_SETTING);
            QTimer *updateTimer = new QTimer(this);
            updateTimer->setInterval(500);
            connect(updateTimer, &QTimer::timeout, [updateTimer, this](){
                qDebug() << value(QLatin1String("Registered")).toBool();
                if (value(QLatin1String("Registered"), Snore::LOCAL_SETTING).toBool()) {
                    updateLoginState();
                    updateTimer->deleteLater();
                }
            });

            updateTimer->start();
        }else{
            setValue(QLatin1String("Registered"), false, Snore::LOCAL_SETTING);
            updateLoginState();
        }
    });
}

PushoverSettings::~PushoverSettings()
{
}

void PushoverSettings::load()
{
    m_deviceLineEdit->setText(value(QLatin1String("DeviceName"), Snore::LOCAL_SETTING).toString());
}

void PushoverSettings::save()
{
}

void PushoverSettings::updateLoginState()
{
    if (value(QLatin1String("Registered"), Snore::LOCAL_SETTING).toBool()) {
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
}
