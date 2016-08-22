#ifndef QVNCVIEWERSETTINGS_H
#define QVNCVIEWERSETTINGS_H

#include <QApplication>
#include <QSettings>
#include <QMdiArea>
#include <QStyle>
#include <QSize>
#include <QFont>
#include <QLocale>

#include "macros.h"

class QtVncViewerSettings : public QSettings
{
    Q_OBJECT

public:
    QMap<QString, QLocale::Language> languageMap;

    QtVncViewerSettings() : QSettings(QSettings::IniFormat, QSettings::UserScope, QVNCVIEWER_APP_NAME)
    {
        languageMap["de"] = QLocale::German;
        languageMap["us"] = QLocale::English;
    }

    ~QtVncViewerSettings()
    {
        sync();
    }

    QString languageToString(QLocale::Language lang)
    {
        QString langStr = languageMap.key(lang);
        if ( !langStr.isEmpty() )
            return langStr;
        else
            return "us";
    }

    QLocale::Language languageFromString(QString lang)
    {
        if ( languageMap.contains(lang) )
            return languageMap[lang];
        else
            return QLocale::English;
    }

public slots:
    // General
    void setApplicationVersion(QString version) { setValue("Version", version); }
    QString applicationVersion() { return value("Version", QVNCVIEWER_APP_VERSION).toString(); }

    // Preferences
    void setPreferencesGuiStyle(QString style) { setValue("Preferences/GuiStyle", style); }
    QString preferencesGuiStyle() { return value("Preferences/GuiStyle", qApp->style()->objectName()).toString(); }
    void setPreferencesAppFont(QString font) { setValue("Preferences/AppFont", font); }
    QString preferencesAppFont() { return value("Preferences/AppFont", qApp->font().toString()).toString(); }
    void setPreferencesAppFontSize(int size) { setValue("Preferences/AppFontSize", size); }
    int preferencesAppFontSize() { return value("Preferences/AppFontSize", qApp->font().pointSize()).toInt(); }
    void setPreferencesMaximizeWindows(bool enable) { setValue("Preferences/MaximizeWindows", enable); }
    bool preferencesMaximizeWindows() { return value("Preferences/MaximizeWindows", true).toBool(); }
    void setPreferencesQuiet(bool enable) { setValue("Preferences/Quiet", enable); }
#if defined(QVNCVIEWER_OS_WIN)
    bool preferencesQuiet() { return value("Preferences/Quiet", true).toBool(); }
#else
    bool preferencesQuiet() { return value("Preferences/Quiet", false).toBool(); }
#endif
    void setPreferencesLanguage(QString lang) { setValue("Preferences/Language", lang); }
    QString preferencesLanguage() { return value("Preferences/Language", languageToString(QLocale::system().language())).toString(); }
    void setPreferencesNativeFileDialogs(bool enable) { setValue("Preferences/NativeFileDialogs", enable); }
#if defined(QVNCVIEWER_OS_MAC)
    bool preferencesNativeFileDialogs() { return value("Preferences/NativeFileDialogs", true).toBool(); }
#else
    bool preferencesNativeFileDialogs() { return value("Preferences/NativeFileDialogs", false).toBool(); }
#endif
    void setPreferencesSurfaceType(int type) { setValue("Preferences/SurfaceType", type); }
    int preferencesSurfaceType() { return value("Preferences/SurfaceType", QVNCVIEWER_SURFACE_RASTER).toInt(); }
    void setPreferencesScaled(bool scaled) { setValue("Preferences/Scaled", scaled); }
    bool preferencesScaled() { return value("Preferences/Scaled", true).toBool(); }
    void setPreferencesBilinearFilter(bool bilinear) { setValue("Preferences/BilinearFilter", bilinear); }
    bool preferencesBilinearFilter() { return value("Preferences/BilinearFilter", true).toBool(); }
    void setPreferencesKeepAspect(bool keep) { setValue("Preferences/KeepAspect", keep); }
    bool preferencesKeepAspect() { return value("Preferences/KeepAspect", true).toBool(); }
    void setPreferencesShowFps(bool enable) { setValue("Preferences/ShowFps", enable); }
    bool preferencesShowFps() { return value("Preferences/ShowFps", false).toBool(); }
    void setPreferencesQualityLevel(int level) { setValue("Preferences/QualityLevel", level); }
    int preferencesQualityLevel() { return value("Preferences/QualityLevel", 6).toInt(); }
    void setPreferencesCompressionLevel(int level) { setValue("Preferences/CompressionLevel", level); }
    int preferencesCompressionLevel() { return value("Preferences/CompressionLevel", 6).toInt(); }
    void setPreferencesEncoding(QString encoding) { setValue("Preferences/Encoding", encoding); }
    QString preferencesEncoding() { return value("Preferences/Encoding", "Hextile").toString(); }

    // VncMainWindow
    void setMainWindowState(QByteArray state) { setValue("VncMainWindow/State", state); }
    QByteArray mainWindowState() { return value("VncMainWindow/State", QByteArray()).toByteArray(); }
    void setMainWindowGeometry(QByteArray geom) { setValue("VncMainWindow/Geometry", geom); }
    QByteArray mainWindowGeometry() { return value("VncMainWindow/Geometry", QByteArray()).toByteArray(); }
    void setMainWindowViewMode(int mode) { setValue("VncMainWindow/ViewMode", mode); }
    int mainWindowViewMode() { return value("VncMainWindow/ViewMode", QVNCVIEWER_VIEWMODE_WINDOWED).toInt(); }
    void setMainWindowRecentConnections(QStringList recentConnections) { setValue("VncMainWindow/RecentConnections", recentConnections); }
    QStringList mainWindowRecentConnections() { return value("VncMainWindow/RecentConnections", QStringList()).toStringList(); }
};

#endif // QVNCVIEWERSETTINGS_H
