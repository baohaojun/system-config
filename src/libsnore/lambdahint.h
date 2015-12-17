#ifndef LAMBDAHINT_H
#define LAMBDAHINT_H

#include "snore_exports.h"
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
#include <QVariant>

#include <functional>

namespace Snore
{


/**
 * LambdaHint is a helper type to enable us to inset functions into the hints.
 * This is useful to be able to add hints that might change during runtime.
 *
 * @author Hannah von Reth \<vonreth at kde.org\>
 */

class SNORE_EXPORT LambdaHint
{
public:
    LambdaHint();
    LambdaHint ( std::function< QVariant() > function );

    QVariant operator() ();

private:
    std::function<QVariant() > m_func;
};

}

Q_DECLARE_METATYPE ( Snore::LambdaHint )

#endif // LAMBDAHINT_H
