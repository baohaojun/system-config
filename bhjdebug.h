#ifndef __BHJDEBUG_H__
#define __BHJDEBUG_H__

#ifdef ENABLE_BHJDEBUG
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <windows.h>
//#include <pthread.h>
#define pthread_self GetCurrentThreadId
#define pthread_t int
#define PATH_SEPW L'\\'
#define PATH_SEP '\\'

#define WIDEN2(x) L ## x
#define WIDEN(x) WIDEN2(x)
#define __WFILE__ WIDEN(__FILE__)
#define __WFUNCTION__ WIDEN(__FUNCTION__)
#define BHJ_LOG_FILENAME "c:\\cygwin\\tmp\\screen-exchange"

#define BHJDEBUG_START() do {                   \
        system("bash -c \"rm /tmp/screen-exchange*\""); \
    } while (0)
    

#define BHJDEBUGW(fmt, ...) do {\
        wprintf(L"%s %s() %d: " fmt L"\n", wcsrchr(__WFILE__, PATH_SEPW)?wcsrchr(__WFILE__, PATH_SEPW)+1:__WFILE__, \
                __WFUNCTION__, __LINE__,                              \
               ##__VA_ARGS__);\
        fflush(stderr);\
    } while(0)

#define BHJDEBUG(fmt, ...) do {                                         \
        pthread_t tmp = pthread_self();                                 \
        char buff[1024] = {0};                                          \
        sprintf(buff, "%s.%x", BHJ_LOG_FILENAME, tmp);                  \
        FILE *fp = fopen(buff, "ab");                                   \
        if (!fp) {                                                      \
            fp = fopen(buff, "wb");                                     \
        }                                                               \
        fprintf(fp, "%s:%d: %s() bhj %x " fmt "\n", strrchr(__FILE__, PATH_SEP)?strrchr(__FILE__, PATH_SEP)+1:__FILE__, \
                __LINE__, __FUNCTION__, tmp,                            \
                ##__VA_ARGS__);                                         \
        fclose(fp);                                                     \
        fprintf(stderr, "%s:%d: %s() bhj %x " fmt "\n", strrchr(__FILE__, PATH_SEP)?strrchr(__FILE__, PATH_SEP)+1:__FILE__, \
               __LINE__, __FUNCTION__, tmp,                             \
               ##__VA_ARGS__);                                          \
        fflush(stderr);                                                 \
    } while(0)

#define simple_debug(a, b) BHJDEBUG(#a " is " #b, a)
#define dsimple_debug(a) simple_debug(a, %d)
#define xsimple_debug(a) simple_debug(a, %x)



#ifdef __cplusplus
class CEnterLeaveDebug {
public:
    char *m_file;
    char *m_func;
    int m_line;
    CEnterLeaveDebug(const char * file, const char *func, int line) {
        m_file = strdup(strrchr(file, PATH_SEP)?strrchr(file, PATH_SEP)+1:file);
        m_func = strdup(func);
        m_line = line;
        pthread_t tmp = pthread_self();
        char buff[1024];
        sprintf(buff, "%s.%x", BHJ_LOG_FILENAME, tmp);
        FILE *fp = fopen(buff, "ab");                        
        if (!fp) {
            fp = fopen(buff, "wb");
        }

        fprintf(fp, "%s:%d: bhj %x entering %s\n", m_file, line, tmp, func);
        fclose(fp);
        fprintf(stderr, "%s:%d: bhj %x entering %s\n", m_file, line, tmp, func);
        fflush(stderr);
    };
    
    ~CEnterLeaveDebug() {
        pthread_t tmp = pthread_self();
        char buff[1024];
        sprintf(buff, "%s.%x", BHJ_LOG_FILENAME, tmp);
        FILE *fp = fopen(buff, "ab");
        if (!fp) {
            fp = fopen(buff, "wb");
        }
        fprintf(fp, "%s:%d: bhj %x leaving %s\n", m_file, m_line, tmp, m_func);
        fclose(fp);

        fprintf(stderr, "%s:%d: bhj %x leaving %s\n", m_file, m_line, tmp, m_func);
        fflush(stderr);
        free(m_func);
        free(m_file);
    };
};

#define EnterLeaveDebug() CEnterLeaveDebug EnterLeave##__LINE__(__FILE__, __FUNCTION__, __LINE__)

#else

#define EnterLeaveDebug()

#endif


//#if 1

#else 
#define BHJDEBUG_START(...)
#define BHJDEBUG(...)
#define simple_debug(...)
#define dsimple_debug(...)
#define xsimple_debug(...)
#define EnterLeaveDebug(...)
#endif
#endif
