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
#ifndef UTILS_H
#define UTILS_H

#include "snore_exports.h"
#include "snoreglobals.h"

#include <QImage>
#include <QObject>

namespace Snore
{
class SNORE_EXPORT  Utils : public QObject
{
    Q_OBJECT
public:
    /**
     * The MARKUP_FLAG enum.
     * If a falg is not present the markup key will be removed.
     * If any flag is present, special characters mus be html escaped.
     */
    enum MARKUP_FLAG {
        /**
         * No markup is supported.
         * All markup will be removed.
         */
        NO_MARKUP   = 0,

        /**
         * Urls are supprotet.
         * &lt;a href="www.foo.bar"&gt;Foo Bar&lt;/a&gt;
         */
        HREF        = 1 << 0,

        /**
         * Line breeaks &lt;br&gt; are supprotet.
         * If the flag is not present &lt;br&gt; will be replaced by \\n
         */
        BREAK       = 1 << 1,

        /**
         * Bold &lt;b&gt; is supportet.
         */
        BOLD        = 1 << 2,

        /**
         * Italic &lt;i&gt; is supportet.
         */
        ITALIC      = 1 << 3,

        /**
         * Underline &lt;u&gt; is supportet.
         */
        UNDERLINE   = 1 << 4,

        /**
         * Fonst are supportet.
         * &lt;font color="blue"&gt; word &lt;/font&gt;
         */
        FONT        = 1 << 5,

        /**
         * All markup is supported.
         * No markup will be removed.
         */
        ALL_MARKUP  = ~0
    };

    Q_DECLARE_FLAGS(MARKUP_FLAGS, MARKUP_FLAG)

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
     * Removes unsupported markup tags from a string.
     */
    static QString normalizeMarkup(QString string, MARKUP_FLAGS tags);

    /**
     * Version number prefix for the settings.
     */
    static inline QString settingsVersionSchema()
    {
        return QStringLiteral("v1");
    }

    /**
     * Returns the SettingsKey formated with type and version.
     * @param key The key.
     * @param type The Type.
     * @param application The application's name.
     */
    static inline QString normalizeSettingsKey(const QString &key, SettingsType type, const QString &application)
    {
        if (type == LOCAL_SETTING) {
            return settingsVersionSchema() + QLatin1String("/LocalSettings/") + application + QLatin1Char('/') + key;
        } else {
            return settingsVersionSchema() + QLatin1String("/GlobalSettings/") +  key;
        }
    }

    static QByteArray dataFromImage(const QImage &image);
private:
#ifdef Q_OS_WIN
    static int attatchToActiveProcess();
    static void detatchActiveProcess(int idActive);
#endif

};

}
Q_DECLARE_OPERATORS_FOR_FLAGS(Snore::Utils::MARKUP_FLAGS)

#endif // UTILS_H
