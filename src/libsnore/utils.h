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
#ifndef UTILS_H
#define UTILS_H

#include "snore_exports.h"
#include "snoreglobals.h"

#include <QApplication>
#include <QCryptographicHash>
#include <QSettings>

namespace Snore
{
class SNORE_EXPORT  Utils : public QObject
{
    Q_OBJECT
public:
    enum MARKUP_FLAG{
        NO_MARKUP   = 0,
        HREF        = 1 << 0,
        BREAK       = 1 << 1,
        BOLD        = 1 << 2,
        ITALIC      = 1 << 3,
        UNDERLINE   = 1 << 4,
        FONT        = 1 << 5,
        ALL_MARKUP  = ~0
    };

    Q_DECLARE_FLAGS(MARKUP_FLAGS,MARKUP_FLAG)

    Utils(QObject *parent = nullptr);
    ~Utils();

    /**
     * Raise a window to the front and activates it.
     * @param wid the Id of the window to raise.
     * @param focus whether the window should request focus.
     */
    //TODO: make Wid usable with the meta system and change signature.
    Q_INVOKABLE static void bringWindowToFront(qlonglong wid, bool focus);

    /**
     * Raised the Window to front and don't make it active or steal focus.
     */
    Q_INVOKABLE static void raiseWindowToFront(qlonglong wid);

    /**
     *
     * @param string A string to encode if needed.
     * @return if the string was rhichtext html encode string is returnd otherwise the original string.
     */
    static QString normalizeMarkup(QString string, MARKUP_FLAGS tags);

    /**
     * Computes a md5 hash of the provided data.
     */
    static inline QString computeMD5Hash(const QByteArray &data)
    {
        return QString::fromUtf8(QCryptographicHash::hash(data, QCryptographicHash::Md5).toHex());
    }

    static inline QString settingsVersionSchema()
    {
        return QLatin1String("v1");
    }

    static inline QString normalizeSettingsKey(const QString &key, SettingsType type, const QString &application)
    {
        if (type == LOCAL_SETTING) {
            return settingsVersionSchema() + QLatin1String("/LocalSettings/") + application + QLatin1Char('/') + key;
        } else {
            return settingsVersionSchema() + QLatin1String("/GlobalSettings/") +  key;
        }
    }

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

private:
#ifdef Q_OS_WIN
    static int attatchToActiveProcess();
    static void detatchActiveProcess(int idActive);
#endif

};

}
Q_DECLARE_OPERATORS_FOR_FLAGS(Snore::Utils::MARKUP_FLAGS)

#endif // UTILS_H
