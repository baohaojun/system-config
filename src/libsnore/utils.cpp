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

void Utils::bringWindowToFront(qlonglong _wid, bool focus)
{
    snoreDebug(SNORE_DEBUG) << _wid;
#ifdef Q_OS_WIN
    HWND wid = (HWND)_wid;
    int idActive = GetWindowThreadProcessId(GetForegroundWindow(), NULL);
    bool attetched = AttachThreadInput(GetCurrentThreadId(), idActive, TRUE);
    SetForegroundWindow(wid);
    if (focus) {
        SetFocus(wid);
    }
    if (attetched) {
        AttachThreadInput(GetCurrentThreadId(), idActive, FALSE);
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


