#ifndef UTILS_H
#define UTILS_H

#include "snoreserver.h"

class SNORE_EXPORT Utils
{
public:
    Utils();
    static QString notificationToSNTPString ( QSharedPointer<Notification> notification );
};

#endif // UTILS_H
