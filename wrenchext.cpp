#include "wrenchext.h"
#include <QDebug>
#include <stdio.h>

WrenchExt::WrenchExt() :
    L(NULL), mUseQtPop(true)
{

}

void WrenchExt::reloadLuaScriptInternally()
{
    if (L) {
        ::lua_close(L);
    }

    L = ::luaL_newstate();
    ::luaL_openlibs(L);

    int error = luaL_loadstring(L, "wrench = require('wrench-ext')") || lua_pcall(L, 0, 0, 0);
    if (error) {
        qDebug() << "WrenchExt init error: " << QString().sprintf("Can't load wrench: %s", lua_tolstring(L, -1, NULL));
        lua_close(L);
        L = NULL;
        return;
    }

    lua_getglobal(L, "wrench");
}

void WrenchExt::reloadLuaScript()
{
    reloadLuaScriptInternally();
    QString pop = callLuaFunc("should_use_internal_pop", QStringList());
    if (pop == "0") {
        mUseQtPop = false;
    } else {
        mUseQtPop = true;
    }
}

bool WrenchExt::shouldUseInternalPop()
{
    return mUseQtPop;
}

bool WrenchExt::isUsefulNotification(const QString &key, const QString &pkg, const QString &title, const QString &text)
{
    QString res = callLuaFunc("is_useful_notification", QStringList() << key << pkg << title << text);
    return res == "1";
}

QString WrenchExt::callLuaFunc(const QString &func, const QStringList &args)
{
    if (L == NULL) {
        reloadLuaScript();
    }

    if (L == NULL) {
        return "";
    }

    lua_getfield(L, -1, func.toUtf8().constData());

    foreach(const QString & arg, args) {
        lua_pushstring(L, arg.toUtf8().constData());
    }

    int error = lua_pcall(L, args.length(), 1, 0);

    if (error) {
        mErrorString = QString("WrenchExt pcall error: ") + QString().sprintf("Can't call is_useful_notification: %s", lua_tolstring(L, -1, NULL));
        qDebug() << mErrorString;
        reloadLuaScriptInternally();
        return "";
    }

    QString res = "";
    if (::lua_isnumber(L, -1) || ::lua_isstring(L, -1)) {
        res = lua_tostring(L, -1);
    }
    lua_pop(L, 1);
    return res;
}

WrenchExt::~WrenchExt()
{
    if (L) {
        lua_close(L);
        L = NULL;
    }
}
