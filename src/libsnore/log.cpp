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

class LogData
{
public:
    int debugLevel = 0;
    QTextStream *logFile = nullptr;
    QFile *file = nullptr;

    LogData()
    {
        debugLevel = qgetenv("LIBSNORE_DEBUG_LVL").toInt();
        if (qgetenv("LIBSNORE_LOG_TO_FILE").toInt() == 1) {

            QString name = QDir::tempPath() + QStringLiteral("/libsnore/") +
                           (qApp->applicationName().isEmpty() ? QString::number(qApp->applicationPid()) : qApp->applicationName()) +
                           QLatin1String("-log.txt");
            if (!qEnvironmentVariableIsSet("LIBSNORE_LOGFILE")) {
                name = QString::fromUtf8(qgetenv("LIBSNORE_LOGFILE"));
            } else {
                QDir::temp().mkpath(QStringLiteral("libsnore"));
            }
            std::cout << "Started logging to " << name.toLocal8Bit().constData() << std::endl;

            file = new QFile(name);
            if (!file->open(QFile::WriteOnly)) {
                qFatal("Failed to open log file %s", qPrintable(name));
            }
            logFile = new QTextStream(file);
        }
    }

    ~LogData()
    {
        delete logFile;
        delete file;
    }

};

Q_GLOBAL_STATIC(LogData, s_data)

SnoreLog::SnoreLog(SnoreDebugLevels lvl):
    QDebug(&m_msg),
    m_lvl(lvl)
{
}

SnoreLog::~SnoreLog()
{
    QByteArray message = QTime::currentTime().toString(QStringLiteral("hh:mm:ss: ")).toUtf8().constData();
    message.append(m_msg.toUtf8().constData());
    if (s_data->logFile) {
        *s_data->logFile << message.constData() << "\n";
        s_data->logFile->flush();
    }
    if (m_lvl == SNORE_WARNING) {
        std::wcerr << message.constData() << std::endl;
    } else  if (s_data->debugLevel >= m_lvl) {
        std::wcout << message.constData() << std::endl;
    }
}

void SnoreLog::setDebugLvl(int i)
{
    s_data->debugLevel = i;
}
