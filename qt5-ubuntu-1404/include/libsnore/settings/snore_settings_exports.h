
#ifndef SNORE_SETTINGS_EXPORT_H
#define SNORE_SETTINGS_EXPORT_H

#ifdef LIBSNORESETTINGS_STATIC_DEFINE
#  define SNORE_SETTINGS_EXPORT
#  define LIBSNORESETTINGS_NO_EXPORT
#else
#  ifndef SNORE_SETTINGS_EXPORT
#    ifdef libsnoresettings_EXPORTS
        /* We are building this library */
#      define SNORE_SETTINGS_EXPORT __attribute__((visibility("default")))
#    else
        /* We are using this library */
#      define SNORE_SETTINGS_EXPORT __attribute__((visibility("default")))
#    endif
#  endif

#  ifndef LIBSNORESETTINGS_NO_EXPORT
#    define LIBSNORESETTINGS_NO_EXPORT __attribute__((visibility("hidden")))
#  endif
#endif

#ifndef LIBSNORESETTINGS_DEPRECATED
#  define LIBSNORESETTINGS_DEPRECATED __attribute__ ((__deprecated__))
#  define LIBSNORESETTINGS_DEPRECATED_EXPORT SNORE_SETTINGS_EXPORT __attribute__ ((__deprecated__))
#  define LIBSNORESETTINGS_DEPRECATED_NO_EXPORT LIBSNORESETTINGS_NO_EXPORT __attribute__ ((__deprecated__))
#endif

#define DEFINE_NO_DEPRECATED 0
#if DEFINE_NO_DEPRECATED
# define LIBSNORESETTINGS_NO_DEPRECATED
#endif

#endif
