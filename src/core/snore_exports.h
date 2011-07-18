#ifndef SNORE_EXPORT_H
#define SNORE_EXPORT_H
#include <QtGlobal>

#ifdef SNORECORE_DLL
# define SNORE_EXPORT Q_DECL_EXPORT
#else
# define SNORE_EXPORT Q_DECL_IMPORT
#endif

#ifndef SNORE_DEPRECATED
# ifdef Q_CC_GNU
#  define SNORE_DEPRECATED __attribute__((deprecated))
# elif defined(Q_CC_MSVC)
#  define SNORE_DEPRECATED __declspec(deprecated)
# else
# define SNORE_DEPRECATED
# endif
#endif

#endif//SNORE_EXPORT_H
