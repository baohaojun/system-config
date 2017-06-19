#ifndef SNORE_VERSION_H
#define SNORE_VERSION_H

#undef major
#undef minor

#include <QString>

namespace Snore
{

/**
 * Version contains relevant version informations.
 * @author Hannah von Reth \<vonreth at kde.org\>
 */
class Version
{
public:
    /**
     *
     * @return the version "major().minor().patch()"
     */
    static const QString version()
    {
        return QStringLiteral("0.7.0");
    }

    /**
     *
     * @return the major version
     */
    static const QString major()
    {
        return QStringLiteral("0");
    }

    /**
    *
     * @return the minor version
     */
    static const QString minor()
    {
        return QStringLiteral("7");
    }

    /**
     *
     * @return the patch version
     */
    static const QString patch()
    {
        return QStringLiteral("0");
    }

    /**
     *
     * @return the git revision, can be empty in a release
     */
    static const QString revision()
    {
        return QStringLiteral("");
    }

};

}
#endif
