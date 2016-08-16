#ifndef MACROS_H
#define MACROS_H

#include <Qt>

// global OS macros for supported target operating systems
#if (defined(Q_OS_UNIX) || defined(Q_OS_LINUX)) && !defined(Q_OS_MAC)
#define QVNCVIEWER_OS_UNIX
#if defined(Q_OS_LINUX)
#define QVNCVIEWER_OS_LINUX
#define QVNCVIEWER_OS_NAME                  QString("Linux")
#else
#define QVNCVIEWER_OS_NAME                  QString("UNIX")
#endif
#elif defined(Q_OS_MAC)
#define QVNCVIEWER_OS_MAC
#define QVNCVIEWER_OS_NAME                  QString("Darwin")
#elif defined(Q_OS_WIN32) || defined(Q_OS_WIN64)
#define QVNCVIEWER_OS_WIN
#define QVNCVIEWER_OS_NAME                  QString("Windows")
#else
#warning "Target OS is not supported -- Qt VNC Viewer currently supports Linux/UNIX, Windows and Mac OS X!"
#define QVNCVIEWER_OS_UNKNOWN
#define QVNCVIEWER_OS_NAME                  QString("Unknown")
#endif

// make a string out of a non-string constant
#define QVNCVIEWER_STR(s)                   #s
#define QVNCVIEWER_XSTR(s)                  QVNCVIEWER_STR(s)

// one second time (milliseconds)
#define QVNCVIEWER_ONE_SECOND               1000

// app name, title, version, ...
#define QVNCVIEWER_APP_NAME                 QString("qvncviewer")
#define QVNCVIEWER_APP_TITLE_CSTR           "Qt VNC Viewer"
#define QVNCVIEWER_APP_TITLE                QString(QVNCVIEWER_APP_TITLE_CSTR)
#define QVNCVIEWER_APP_VERSION              QString(QVNCVIEWER_XSTR(QVNCVIEWER_VERSION))
#define QVNCVIEWER_ORG_DOMAIN               QString("batcom-it.net")
#define QVNCVIEWER_ORG_NAME                 QString("qvncviewer")

// dot-path related
#if defined(Q_OS_MAC)
#define QVNCVIEWER_DOT_PATH                 (QDir::homePath() + "/Library/Application Support/qvncviewer")
#else
#define QVNCVIEWER_DOT_PATH                 (QDir::homePath() + "/.qvncviewer")
#endif
#define QVNCVIEWER_DYN_DOT_PATH             (qApp->arguments().indexOf("-config_path") >= 0 && qApp->arguments().indexOf("-config_path") + 1 <= qApp->arguments().count() ? qApp->arguments()[qApp->arguments().indexOf("-config_path") + 1]: QVNCVIEWER_DOT_PATH)

// other arguments
#define QVNCVIEWER_ARG_FULLSCREEN           (qApp->arguments().indexOf("-fullscreen") >= 0)
#define QVNCVIEWER_ARG_MAXIMIZE             (qApp->arguments().indexOf("-maximize") >= 0)
#define QVNCVIEWER_ARG_QUIET                (qApp->arguments().indexOf("-quiet") >= 0)
#define QVNCVIEWER_ARG_RASTER               (qApp->arguments().indexOf("-raster") >= 0)
#define QVNCVIEWER_ARG_OPENGL               (qApp->arguments().indexOf("-opengl") >= 0)

// view modes
#define QVNCVIEWER_VIEWMODE_WINDOWED        QMdiArea::SubWindowView
#define QVNCVIEWER_VIEWMODE_TABBED          QMdiArea::TabbedView

// connection window: menu-bar visibility timeout (when the window is in full screen mode)
#define QVNCVIEWER_MENUBAR_TIMEOUT          QVNCVIEWER_ONE_SECOND

// connection window: menu-bar mouse y-position (in pixels) - the menu will be shown when the mouse cursor's y-position is below this value (when the window is in full screen mode)
#define QVNCVIEWER_MENUBAR_MOUSE_Y          10

// delay before hostname lookups while editing
#define QVNCVIEWER_HOSTEDIT_TIMEOUT         100

// "connection pending" timeout in milliseconds
#define QVNCVIEWER_CONNPEND_TIMEOUT         100

// max. number of "recent connections"
#define QVNCVIEWER_MAX_RECENT_CONNECTIONS   10

// key sequences
#define QVNCVIEWER_FULLSCREEN_TOGGLED       (keyEvent->modifiers() & Qt::AltModifier) && (keyEvent->key() == Qt::Key_Enter || keyEvent->key() == Qt::Key_Return)

// RFB related
#define QVNCVIEWER_BITS_PER_SAMPLE          8
#define QVNCVIEWER_SAMPLES_PER_PIXEL        1
#define QVNCVIEWER_BYTES_PER_PIXEL          4
#define QVNCVIEWER_VNC_BASE_PORT            5900

// surface implementations
#define QVNCVIEWER_SURFACE_RASTER           0
#define QVNCVIEWER_SURFACE_OPENGL           1

// debugging macros
#define QVNCVIEWER_PRINT_TXT(t)             { printf("%s\n", #t); fflush(stdout); }
#define QVNCVIEWER_PRINT_STR(s)             { printf("%s = %s\n", #s, s.toLocal8Bit().constData()); fflush(stdout); }
#define QVNCVIEWER_PRINT_CSTR(s)            { printf("%s = %s\n", #s, s); fflush(stdout); }
#define QVNCVIEWER_PRINT_PTR(p)             { printf("%s = %p\n", #p, p); fflush(stdout); }
#define QVNCVIEWER_PRINT_INT(i)             { printf("%s = %ld\n", #i, i); fflush(stdout); }
#define QVNCVIEWER_PRINT_HEX(x)             { printf("%s = %x\n", #x, x); fflush(stdout); }
#define QVNCVIEWER_PRINT_REAL(r)            { printf("%s = %f\n", #r, r); fflush(stdout); }
#define QVNCVIEWER_PRINT_BOOL(b)            { printf("%s = %s\n", #b, b ? "true" : "false"); fflush(stdout); }
#define QVNCVIEWER_PRINT_STRLST(l)          { for (int i = 0; i < l.count(); i++) printf("%s[%ld] = %s\n", #l, i, (const char *)l[i].toLocal8Bit()); fflush(stdout); }

#endif // MACROS_H
