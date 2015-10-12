/*
  SnoreNotify is a Notification Framework based on Qt
  Copyright (C) 2014-2015  Hannah von Reth <vonreth@kde.org>

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

#include <QApplication>
#include <QTime>
#include <QDir>
#include <QTextStream>

#include <iostream>
#include <memory>
#include <sstream>

using namespace Snore;

static int debugLevel = 0;
static std::unique_ptr<QTextStream> logFile = std::unique_ptr<QTextStream>();
static std::unique_ptr<QFile> file = std::unique_ptr<QFile>();

static void init()
{
    debugLevel = qgetenv("LIBSNORE_DEBUG_LVL").toInt();
    if (qgetenv("LIBSNORE_LOG_TO_FILE").toInt() == 1) {

        QString name = QDir::tempPath() + QLatin1String("/libsnore/") +
                       (qApp->applicationName().isEmpty() ? QString::number(qApp->applicationPid()) : qApp->applicationName()) +
                       QLatin1String("-log.txt");
        if (!qgetenv("LIBSNORE_LOGFILE").isNull()) {
            name = QString::fromUtf8(qgetenv("LIBSNORE_LOGFILE"));
        } else {
            QDir::temp().mkpath(QLatin1String("libsnore"));
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
    QByteArray message = QTime::currentTime().toString(QLatin1String("hh:mm:ss: ")).toUtf8().constData();
    message.append(m_msg.toUtf8().constData());
    if (logFile) {
        *logFile << message.constData() << "\n";
        logFile->flush();
    }
    if (m_lvl == SNORE_WARNING) {
        std::wcerr << message.constData() << std::endl;
    } else  if (debugLevel >= m_lvl) {
        std::wcout << message.constData() << std::endl;
    }
}

void SnoreLog::setDebugLvl(int i)
{
    debugLevel = i;
}
