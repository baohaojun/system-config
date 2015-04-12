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

using namespace Snore;

void Utils::bringWindowToFront(qlonglong _wid)
{
    snoreDebug(SNORE_DEBUG) << _wid;
#ifdef Q_OS_WIN
    HWND wid = (HWND)_wid;
    HWND hwndActiveWin = GetForegroundWindow();
    int idActive = GetWindowThreadProcessId(hwndActiveWin, NULL);

    if (AttachThreadInput(GetCurrentThreadId(), idActive, TRUE)) {
        SetForegroundWindow(wid);
        SetFocus(wid);
        AttachThreadInput(GetCurrentThreadId(), idActive, FALSE);
    } else {
        // try it anyhow
        SetForegroundWindow(wid);
        SetFocus(wid);
    }
#endif
}

Utils::Utils(QObject *parent):
    QObject(parent)
{

}

Utils::~Utils()
{

}


