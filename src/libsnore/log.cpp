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
#include <QDir>
#include <QApplication>
#include <QTextStream>

using namespace Snore;

class Loger
{
public:
    static int s_debugLevel;

    static inline int debugLvl()
    {
        if (s_debugLevel == -1) {
            s_debugLevel = qgetenv("LIBSNORE_DEBUG_LVL").toInt();
        }
        return s_debugLevel;
    }

    static inline bool isLogToFileEnabled()
    {
        static int s_logToFile = -1;
        if (s_logToFile == -1) {
            s_logToFile = qgetenv("LIBSNORE_LOG_TO_FILE").toInt();
        }
        return s_logToFile == 1;
    }

    static inline QTextStream &logFile()
    {
        static QTextStream *s_out = nullptr;
        static QFile *s_file = nullptr;
        if (!s_out) {
            QString name = QString("%1/libsnore/%2-log.txt").arg(QDir::tempPath(), qApp->applicationName().isEmpty() ? QString::number(qApp->applicationPid()) : qApp->applicationName());
            if (!qgetenv("LIBSNORE_LOGFILE").isNull()) {
                name = QString(qgetenv("LIBSNORE_LOGFILE"));
            } else {
                QDir::temp().mkpath("libsnore");
            }
            std::cout << "Started logging to " << name.toLocal8Bit().constData() << std::endl;

            s_file = new QFile(name);
            if (!s_file->open(QFile::WriteOnly)) {
                qFatal("Failed to open log file %s", qPrintable(name));
            }
            s_out = new QTextStream(s_file);
        }
        return *s_out;
    }

};

int Loger::s_debugLevel = -1;

SnoreLog::SnoreLog(SnoreDebugLevels lvl):
    QDebug(&m_msg),
    m_lvl(lvl)
{
}

SnoreLog::~SnoreLog()
{
    if (Loger::isLogToFileEnabled()) {
        Loger::logFile() << m_msg << "\n";
        Loger::logFile().flush();
    }
    if (m_lvl == SNORE_WARNING) {
        std::cerr << m_msg.toLocal8Bit().constData() << std::endl;
    } else  if (Loger::debugLvl() >= m_lvl) {
        std::cout << m_msg.toLocal8Bit().constData() << std::endl;
    }
}

void SnoreLog::setDebugLvl(int i)
{
    Loger::s_debugLevel = i;
}
