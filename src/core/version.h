#ifndef VERSION_H
#define VERSION_H

#include "snore_exports.h"

#include <QString>

namespace Snore{

class SNORE_EXPORT Version{
public:
static const QString major();

static const QString minor();

static const QString suffix();

static const QString revision();

};

}
#endif