#ifndef _T1WRENCH_LOG_H_
#define _T1WRENCH_LOG_H_

#ifndef LOG_TAG
#define LOG_TAG NULL
#endif

#include <android/log.h>

#define LOG_FATAL_IF(cond, ...) \
    ( (cond) \
    ? ((void)__android_log_assert(#cond, LOG_TAG, ## __VA_ARGS__)) \
    : (void)0 )

#endif /* _T1WRENCH_LOG_H_ */
