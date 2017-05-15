// -*- mode: c++ -*-
#ifndef _WRENCHEXT_H_
#define _WRENCHEXT_H_

#include <QString>
#include <QObject>

#include "lua.hpp"
#include <QStringList>

class WrenchExt : public QObject {
    Q_OBJECT
public:
    bool isUsefulNotification(const QString& key, const QString& pkg, const QString& title, const QString& text, const QString& ticker);
    QString reWriteNotificationText(const QString& key, const QString& pkg, const QString& title, const QString& text, const QString& ticker);
    void reloadLuaScript();
    bool shouldUseInternalPop();
    WrenchExt();
    ~WrenchExt();
    QString callLuaFunc(const QString& func, const QStringList& args = QStringList());
    QString getConfig(const QString& config);
private:
    lua_State *L;
    bool mUseQtPop;

    QString mErrorString;
    void reloadLuaScriptInternally();
signals:
    void luaErrorSig(const QString&);
};

#endif /* _WRENCHEXT_H_ */

