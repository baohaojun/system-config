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

#ifndef LOG_H
#define LOG_H

#include <QDebug>
#include "snore_exports.h"

namespace Snore
{
class SNORE_EXPORT Log : public QDebug
{
public:
    Log(int lvl);
    ~Log();

    static inline int debugLvl()
    {
        return s_debugLevel;
    }

    static setDebugLvl(int i);

private:
    static int s_debugLevel;
    int m_lvl;
    QString m_msg;
};
}

#define snoreDebug(x) Snore::Log( x ) << Q_FUNC_INFO
#define SNORE_DEBUG 3
#define SNORE_INFO 2
#define SNORE_WARNING 1
#endif // LOG_H
