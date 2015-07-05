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
#ifndef SETTINGSDIALOG_H
#define SETTINGSDIALOG_H

#include "snore_exports.h"

#include <QWidget>

class QAbstractButton;

namespace Snore
{
class PluginSettingsWidget;
}

namespace Ui
{
class SettingsDialog;
}

namespace Snore
{
class SNORE_EXPORT SettingsDialog : public QWidget
{
    Q_OBJECT

public:
    explicit SettingsDialog(QWidget *parent = 0);
    ~SettingsDialog();

    //TODO: move to private header
    void initTabs();

public Q_SLOTS:
    void setVisible(bool b) override;
    void accept();
    void reset();

private Q_SLOTS:
    void on_pushButton_clicked();
    void load();
    void save();

private:
    Ui::SettingsDialog *ui;
    QList<Snore::PluginSettingsWidget *> m_tabs;

};
}

#endif // SETTINGSDIALOG_H
