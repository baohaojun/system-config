/*
  SnoreNotify is a Notification Framework based on Qt
  Copyright (C) 2014  Patrick von Reth <vonreth@kde.org>


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

#include "log.h"
#include <iostream>
#include <fstream>

#include <QDir>
#include <QMutex>
#include <QApplication>

using namespace Snore;
int SnoreLog::s_debugLevel = -1;

SnoreLog::SnoreLog(SnoreDebugLevels lvl):
    QDebug(&m_msg),
    m_lvl(lvl)
{
}

SnoreLog::~SnoreLog()
{
    static std::ofstream m_logg(QString("%1/libsnore/%2-%3-log.txt").arg(QDir::tempPath(), qApp->applicationName(), QString::number(qApp->applicationPid())).toUtf8().constData());
    static QMutex m_mutex;
    QMutexLocker lock(&m_mutex);
    if(debugLvl() >= m_lvl)
    {
        std::cout << m_msg.toUtf8().constData() << std::endl;
    }
    m_logg << m_msg.toUtf8().constData() << std::endl;
}

void SnoreLog::setDebugLvl(int i)
{
    s_debugLevel = i;
}
