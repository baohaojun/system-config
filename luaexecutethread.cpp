#include "luaexecutethread.hpp"
#include <QtCore/QString>
#include "bhj_help.hpp"
#include <QtCore/QStringList>
#include <QtCore/QDebug>
#include <QtCore/QProcess>
#include "lua.hpp"
#include <QtWidgets/QMessageBox>

LuaExecuteThread* that;
static int l_selectArg(lua_State* L)
{
    int n = luaL_len(L, -1);
    QStringList args;
    for (int i = 1; i <= n; i++) {
        lua_rawgeti(L, -1, i);
        args << (QString::fromUtf8(lua_tolstring(L, -1, NULL)));
        lua_settop(L, -2);
    }
    QString res = that->selectArgs(args);
    lua_pushstring(L, res.toUtf8().constData());
    return 1;
}

void LuaExecuteThread::run()
{
    L = luaL_newstate();             /* opens Lua */
    luaL_openlibs(L);        /* opens the standard libraries */
    lua_pushcfunction(L, l_selectArg);
    lua_setglobal(L, "select_args");


    int error = luaL_loadstring(L, "t1wrench = require('t1wrench')") || lua_pcall(L, 0, 0, 0);
    if (error) {
        emit gotSomeLog("exit", QString().sprintf("Can't load t1wrench: %s", lua_tolstring(L, -1, NULL)));
        lua_close(L);
        return;
    }

    lua_getglobal(L, "t1wrench");
    while (true) {
        QStringList script;
        mMutex.lock();
        if (mActions.length() == 0) {
            mWait.wait(&mMutex);
        }
        if (mQuit) {
            mMutex.unlock();
            lua_close(L);
            return;
        }

        script = mActions.at(0);
        mActions.removeFirst();
        mMutex.unlock();
        QString func = script.at(0);
        lua_getfield(L, -1, qPrintable(func));
        script.pop_front();
        foreach (const QString& str, script) {
            lua_pushstring(L, str.toUtf8().constData());
        }
        error = lua_pcall(L, script.length(), 1, 0);
        if (error) {
            emit gotSomeLog("exit", QString().sprintf("Can't run %s: %s", qPrintable(func), lua_tolstring(L, -1, NULL)));
            lua_close(L);
            return;
        }
        if (lua_isstring(L, -1)) {
            emit gotSomeLog("info", lua_tolstring(L, -1, NULL));
        }
        lua_pop(L, 1);
    }
}

LuaExecuteThread::LuaExecuteThread(QObject* parent)
    : QThread(parent),
      mQuit(false)
{
}

void LuaExecuteThread::addScript(QStringList script)
{
    extern QString prompt_user(const QString &info, QMessageBox::StandardButtons buttons = QMessageBox::Ok);
    if (!this->isRunning()) {
        prompt_user("后台已停止运行，无法执行此动作，请连接手机或点一下设置按钮");
    }

    if (this != that) {
        that = this;
    }
    mMutex.lock();
    mActions.append(script);
    mMutex.unlock();
    mWait.wakeOne();
}

void LuaExecuteThread::quitLua()
{
    mMutex.lock();
    mQuit = true;
    mMutex.unlock();
    mWait.wakeOne();
}

QString LuaExecuteThread::selectArgs(const QStringList& args)
{
    emit selectArgsSig(args);
    mSelectArgsMutex.lock();
    mSelectArgsWait.wait(&mSelectArgsMutex);
    QString res = mSelectedArg;
    mSelectArgsMutex.unlock();
    return res;
}

void LuaExecuteThread::on_argSelected(const QString& arg)
{
    mSelectArgsMutex.lock();
    mSelectedArg = arg;
    mSelectArgsMutex.unlock();
    mSelectArgsWait.wakeOne();
}
