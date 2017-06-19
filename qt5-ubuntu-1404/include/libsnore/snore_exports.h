
#ifndef SNORE_EXPORT_H
#define SNORE_EXPORT_H

#ifdef LIBSNORE_STATIC_DEFINE
#  define SNORE_EXPORT
#  define LIBSNORE_NO_EXPORT
#else
#  ifndef SNORE_EXPORT
#    ifdef libsnore_EXPORTS
        /* We are building this library */
#      define SNORE_EXPORT __attribute__((visibility("default")))
#    else
        /* We are using this library */
#      define SNORE_EXPORT __attribute__((visibility("default")))
#    endif
#  endif

#  ifndef LIBSNORE_NO_EXPORT
#    define LIBSNORE_NO_EXPORT __attribute__((visibility("hidden")))
#  endif
#endif

#ifndef LIBSNORE_DEPRECATED
#  define LIBSNORE_DEPRECATED __attribute__ ((__deprecated__))
#  define LIBSNORE_DEPRECATED_EXPORT SNORE_EXPORT __attribute__ ((__deprecated__))
#  define LIBSNORE_DEPRECATED_NO_EXPORT LIBSNORE_NO_EXPORT __attribute__ ((__deprecated__))
#endif

#define DEFINE_NO_DEPRECATED 0
#if DEFINE_NO_DEPRECATED
# define LIBSNORE_NO_DEPRECATED
#endif

#endif
