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

#include <memory>

using namespace Snore;

static int debugLevel = 0;
static std::unique_ptr<QTextStream> logFile = std::unique_ptr<QTextStream>();
static std::unique_ptr<QFile> file = std::unique_ptr<QFile>();

static void init()
{
    debugLevel = qgetenv("LIBSNORE_DEBUG_LVL").toInt();
    if (qgetenv("LIBSNORE_LOG_TO_FILE").toInt() == 1) {

        QString name = QString("%1/libsnore/%2-log.txt").arg(QDir::tempPath(), qApp->applicationName().isEmpty() ? QString::number(qApp->applicationPid()) : qApp->applicationName());
        if (!qgetenv("LIBSNORE_LOGFILE").isNull()) {
            name = QString(qgetenv("LIBSNORE_LOGFILE"));
        } else {
            QDir::temp().mkpath("libsnore");
        }
        std::cout << "Started logging to " << name.toLocal8Bit().constData() << std::endl;

        file = std::unique_ptr<QFile>(new QFile(name));
        if (!file->open(QFile::WriteOnly)) {
            qFatal("Failed to open log file %s", qPrintable(name));
        }
        logFile = std::unique_ptr<QTextStream>(new QTextStream(file.get()));
    }
}
Q_COREAPP_STARTUP_FUNCTION(init)

SnoreLog::SnoreLog(SnoreDebugLevels lvl):
    QDebug(&m_msg),
    m_lvl(lvl)
{
}

SnoreLog::~SnoreLog()
{
    if (logFile) {
        *logFile << m_msg << "\n";
        logFile->flush();
    }
    if (m_lvl == SNORE_WARNING) {
        std::cerr << m_msg.toLocal8Bit().constData() << std::endl;
    } else  if (debugLevel >= m_lvl) {
        std::cout << m_msg.toLocal8Bit().constData() << std::endl;
    }
}

void SnoreLog::setDebugLvl(int i)
{
    debugLevel = i;
}
