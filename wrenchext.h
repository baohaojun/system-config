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
    bool isUsefulNotification(const QString& key, const QString& pkg, const QString& title, const QString& text);
    void reloadLuaScript();
    bool shouldUseInternalPop();
    WrenchExt();
    ~WrenchExt();

private:
    lua_State *L;

    QString callLuaFunc(const QString& func, const QStringList& args);
    QString mErrorString;
    void reloadLuaScriptInternally();
signals:
    void luaErrorSig(const QString&);
};

#endif /* _WRENCHEXT_H_ */

