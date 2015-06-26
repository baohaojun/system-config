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
#include "utils.h"
#include "log.h"

#ifdef Q_OS_WIN
#include <windows.h>
#include <windowsx.h>
#include <shellapi.h>
#include <winuser.h>
#endif

#include <QRegExp>
#include <QMutex>
#include <QTextDocument>
#include <QTextDocumentFragment>

using namespace Snore;

Utils::Utils(QObject *parent):
    QObject(parent)
{

}

Utils::~Utils()
{

}

void Utils::bringWindowToFront(qlonglong _wid, bool focus)
{
#ifdef Q_OS_WIN
    HWND wid = (HWND)_wid;
    int active = attatchToActiveProcess();
    SetForegroundWindow(wid);
    if (focus) {
        SetFocus(wid);
    }
    detatchActiveProcess(active);
#else
    Q_UNUSED(_wid);
    Q_UNUSED(focus);
#endif
}

void Utils::raiseWindowToFront(qlonglong wid)
{
    // Looks like qt is handling it on linux.
#ifdef Q_OS_WIN
    int active = attatchToActiveProcess();
    SetWindowPos((HWND)wid, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE);
    detatchActiveProcess(active);
#else
    Q_UNUSED(wid);
#endif
}


#define HTML_REPLACE(STRING, PATTERN){\
    static QRegExp regexp(QLatin1String(PATTERN));\
    STRING = STRING.replace(regexp, QStringLiteral("\\1"));\
    }\


QString Utils::normaliseMarkup(QString string, MARKUP_FLAGS tags)
{
    static QMutex mutex;
    if(tags == ALL_MARKUP){
        return string;
    } else if(tags == NO_MARKUP) {
        if (Qt::mightBeRichText(string)) {
            return QTextDocumentFragment::fromHtml(string).toPlainText();
        }
        return string;
    }

    QMutexLocker lock(&mutex);

    if (~tags & Utils::BREAK) {
        static QRegExp br("<br>");
        string = string.replace(br, "\n");
    }
    if (~tags & Utils::HREF) {
        HTML_REPLACE(string, "<a href=.*>([^<]*)</a>");
    }
    if (~tags & Utils::ITALIC) {
        HTML_REPLACE(string, "<i>([^<]*)</i>");
    }
    if (~tags & Utils::BOLD) {
        HTML_REPLACE(string, "<b>([^<]*)</b>");
    }
    if (~tags & Utils::UNDERLINE) {
        HTML_REPLACE(string, "<u>([^<]*)</u>");
    }
    if (~tags & Utils::FONT) {
        HTML_REPLACE(string, "<font.*>([^<]*)</font>");
    }
    return string;
}

#ifdef Q_OS_WIN
int Utils::attatchToActiveProcess()
{
    int idActive = GetWindowThreadProcessId(GetForegroundWindow(), NULL);
    return AttachThreadInput(GetCurrentThreadId(), idActive, TRUE) ? idActive : -1;
}

void Utils::detatchActiveProcess(int idActive)
{
    if (idActive != -1) {
        AttachThreadInput(GetCurrentThreadId(), idActive, FALSE);
    }
}

#endif
