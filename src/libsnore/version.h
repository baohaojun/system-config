#ifndef SNORE_VERSION_H
#define SNORE_VERSION_H
#undef major
#undef minor

#include "snore_exports.h"

#include <QString>

namespace Snore
{

/**
 * Version contains relevant version informations.
 * @author Patrick von Reth \<vonreth at kde.org\>
 */
class SNORE_EXPORT Version
{
public:
    /**
     *
     * @return the version "major().minor().suffix()"
     */
    static const QString version();

    /**
     *
     * @return the major version
     */
    static const QString major();

    /**
     *
     * @return the minor version
     */
    static const QString minor();

    /**
     *
     * @return the patch version
     */
    static const QString patch();

    /**
     *
     * @return the git revision, can be empty in a release
     */
    static const QString revision();

    /**
     *
     * @return the build time
     */
    static const QString buildTime();

};

}
#endif
