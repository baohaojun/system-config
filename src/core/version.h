#ifndef SNORE_VERSION_H
#define SNORE_VERSION_H
#undef major
#undef minor

#include "snore_exports.h"

#include <QString>

namespace Snore{

class SNORE_EXPORT Version{
public:
static const QString version();

static const QString major();

static const QString minor();

static const QString suffix();

static const QString revision();

};

}
#endif
