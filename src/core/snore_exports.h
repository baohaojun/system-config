#ifndef SNORE_EXPORT_H
#define SNORE_EXPORT_H


#if defined(HAVE_KDE) 
#include <kdemacros.h>
#ifdef SNORECORE_DLL
# define SNORE_EXPORT KDE_EXPORT
#else
# define SNORE_EXPORT KDE_IMPORT
#endif
#else 
#include <QtGlobal>
#ifdef SNORECORE_DLL
# define SNORE_EXPORT Q_DECL_EXPORT
#else
# define SNORE_EXPORT Q_DECL_IMPORT
#endif
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
