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
#include <QTextDocument>
#include <QTextDocumentFragment>

namespace Snore
{
class SNORE_EXPORT  Utils : public QObject
{
    Q_OBJECT
public:
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
     * @param string A string to decode if needed.
     * @return if the string was rhichtext or html encoded a decoded string, else the original string.
     */
    static inline QString toPlainText(const QString &string)
    {
        if (Qt::mightBeRichText(string)) {
            return QTextDocumentFragment::fromHtml(string).toPlainText();
        } else {
            return string;
        }
    }

    /**
     *
     * @param string A string to encode if needed.
     * @return if the string was rhichtext html encode string is returnd otherwise the original string.
     */
    static inline QString toHtml(const QString &string)
    {
        if (Qt::mightBeRichText(string)) {
            return QTextDocumentFragment::fromHtml(string).toHtml("UTF-8");
        } else {
            return string;
        }
    }

    /**
     * Computes a md5 hash of the provided data.
     */
    static inline QString computeMD5Hash(const QByteArray &data)
    {
        return QCryptographicHash::hash(data, QCryptographicHash::Md5).toHex();
    }

    static inline QString settingsVersionSchema()
    {
        return "v1";
    }

    static inline QString normalizeSettingsKey(const QString &key, SettingsType type, const QString &application)
    {
        if (type == LOCAL_SETTING) {
            return QString("%1/LocalSettings/%2/%3").arg(settingsVersionSchema(), application, key);
        } else {
            return QString("%1/GlobalSettings/%2").arg(settingsVersionSchema(), key);
        }
    }

    template<typename Func>
    static QStringList allSettingsKeysWithPrefix(const QString &prefix, QSettings &settings, Func fun)
    {
        QStringList groups = prefix.split("/");
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

#endif // UTILS_H
