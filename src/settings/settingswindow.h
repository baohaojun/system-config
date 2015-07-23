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
#ifndef SETTINGSWINDOW_H
#define SETTINGSWINDOW_H

#include <QMainWindow>
#include <QSettings>

namespace Ui
{
class SettingsWindow;
}

class QAbstractButton;

class SettingsWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit SettingsWindow(QWidget *parent = 0);
    ~SettingsWindow();

    static QStringList knownApps();

    static QSettings &settings();

    template<typename Func>
    static QStringList allSettingsKeysWithPrefix(const QString &prefix, QSettings &settings, Func fun)
    {
        QStringList groups = prefix.split(QLatin1Char('/'));
        QStringList out;

        for (const QString group : groups) {
            settings.beginGroup(group);
        }
        out = fun(settings);

        for (int i = 0; i < groups.size(); ++i) {
            settings.endGroup();
        }
        return out;
    }

private Q_SLOTS:
    void on_buttonBox_clicked(QAbstractButton *button);
    void on_comboBox_currentIndexChanged(const QString &arg1);

private:
    Ui::SettingsWindow *ui;
};

#endif // SETTINGSWINDOW_H
