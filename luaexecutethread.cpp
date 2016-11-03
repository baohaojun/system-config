#include "luaexecutethread.hpp"
#include <QtCore/QString>
#include "bhj_help.hpp"
#include <QtCore/QStringList>
#include <QtCore/QDebug>
#include <QtCore/QProcess>
#include "lua.hpp"
#include <QtWidgets/QMessageBox>
#include <QTcpSocket>
#include "wrench.h"
#include "adbclient.h"

LuaExecuteThread* that;

static QStringList l_getArgs(lua_State* L)
{
    int n = luaL_len(L, -1);
    QStringList args;
    for (int i = 1; i <= n; i++) {
        lua_rawgeti(L, -1, i);
        args << (QString::fromUtf8(lua_tolstring(L, -1, NULL)));
        lua_settop(L, -2);
    }
    return args;
}

static int l_adb_push(lua_State* L)
{
    QStringList args = l_getArgs(L);
    if (args.size() != 2) {
        luaL_argerror(L, 2, "takes 2 argument");
    }

    if (!AdbClient::doAdbPush(args[0], args[1])) {
        lua_pushstring(L, "adb push failed");
        lua_error(L);
    }
    return 0;
}

static int l_adb_pull(lua_State* L)
{
    QStringList args = l_getArgs(L);
    if (args.size() != 2) {
        luaL_argerror(L, 2, "takes 2 argument");
    }

    qDebug() << "adb pull" << args;

    if (!AdbClient::doAdbPull(args[0], args[1])) {
        lua_pushstring(L, ("adb pull failed: " + args[0] + " " + args[1]).toUtf8().constData());
        lua_error(L);
    }
    return 0;
}

static int l_adb_install(lua_State* L)
{
    QStringList args = l_getArgs(L);
    if (args.size() != 1) {
        luaL_argerror(L, 1, "takes 1 argument");
    }

    QString apk = args[0];
    QString tmpApk = QString("/data/local/tmp/") + QFileInfo(apk).fileName();
    qDebug() << "apk is" << apk << "tmpApk is" << tmpApk;
    if (!AdbClient::doAdbPush(apk, tmpApk)) {
        lua_pushstring(L, "adb push apk for install failed");
        lua_error(L);
    }

    QString res = AdbClient::doAdbShell("pm install -r " + tmpApk);
    if (res == "") {
        lua_pushstring(L, "adb install failed");
        lua_error(L);
    }

    lua_pushstring(L, res.toUtf8().constData());
    // pm 'install' '-r' '/data/local/tmp/Setclip.apk'
    return 1;
}

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

static int l_selectApps(lua_State* L)
{
    that->selectApps();
    return 0;
}

static int l_qt_adb_pipe(lua_State* L)
{
    int n = luaL_len(L, -1);
    QStringList args;
    for (int i = 1; i <= n; i++) {
        lua_rawgeti(L, -1, i);
        args << (QString::fromUtf8(lua_tolstring(L, -1, NULL)));
        lua_settop(L, -2);
    }

    QString res = AdbClient::doAdbShell(args);
    lua_pushstring(L, res.toUtf8().constData());
    return 1;
}

static int l_logToUI(lua_State* L)
{
    that->logToUI(lua_tolstring(L, -1, NULL));
    return 0;
}

static int l_is_exiting(lua_State* L)
{
    lua_pushboolean(L, that->isQuit());
    return 1;
}

static int l_adbQuickInputAm(lua_State* L)
{
    int n = luaL_len(L, -1);
    if (n != 1) {
        QString error = QString().sprintf("takes only 1 string, got %d", n);
        luaL_argerror(L, 1, qPrintable(error));
    }

    QString arg;
    lua_rawgeti(L, -1, n);
    arg = QString::fromUtf8(lua_tolstring(L, -1, NULL));
    lua_settop(L, -2);

    QString res = that->adbQuickInputAm(arg);
    lua_pushstring(L, res.toUtf8().constData());
    return 1;
}

static bool t1SockOk(QTcpSocket* t1Sock)
{
    if (t1Sock == NULL) {
        return false;
    }

    t1Sock->write("ping\n");
    t1Sock->flush();
    t1Sock->readLine();
    if (t1Sock->state() != QAbstractSocket::ConnectedState) {
        return false;
    }
    return true;
}

QString LuaExecuteThread::adbQuickInputAm(QString arg)
{
    if (!t1SockOk(t1Sock)) {
        if (t1Sock) {
            delete t1Sock;
            t1Sock = NULL;
            qDebug() << "prepare to reconnect t1Sock";
        }
        qDebug() << "connect to localhost 28888";
        t1Sock = new QTcpSocket();
        t1Sock->connectToHost("localhost", 28888, QIODevice::ReadWrite);
        t1Sock->waitForConnected();
    }

    QString res;
    if (arg.startsWith("input ") || arg.startsWith("sleep ")) {
        QStringList actions = arg.split(";", QString::SkipEmptyParts);
        foreach (const QString& action, actions) {
            qDebug() << "doing action" << action;
            if (action.startsWith("sleep ")) {
                int msec = atof(qPrintable(action.mid(6))) * 1000;
                emit requestSyncScreen();
                QThread::msleep(msec);
                continue;
            }

            t1Sock->write(action.toUtf8() + "\n");
            t1Sock->flush();
            if (!t1Sock->waitForReadyRead(2000)) {
                emit gotSomeLog("info", QString("命令超时： ") + action);
            }
            res = t1Sock->readLine();
            qDebug() << "got result" << res;
        }
    } else if (arg.startsWith("am ")) {
        t1Sock->write(arg.toUtf8() + "\n");
        t1Sock->flush();
        if (!t1Sock->waitForReadyRead(2000)) {
            emit gotSomeLog("info", QString("命令超时： ") + arg);
        }
        res = t1Sock->readLine();
        qDebug() << "got result" << res;
    } else {
        QString error = "Invalid t1sock input: " + arg;
        luaL_argerror(L, 1, qPrintable(error));
    }

    return res;
}

void LuaExecuteThread::setVariableLocked(const QString& name, const QString& val)
{
    mVariableMutex.lock();
    mVariableHash[name] = val;
    mVariableMutex.unlock();
}

QString LuaExecuteThread::getVariableLocked(const QString& name, const QString& defaultVal)
{
    QString res;
    mVariableMutex.lock();
    if (mVariableHash.contains(name)) {
        res = mVariableHash[name];
    } else {
        res = defaultVal;
    }
    mVariableMutex.unlock();
    return res;
}

//f:write(('t1_load_mail_heads([[%s]], [[%s]], [[%s]], [[%s]], [[%s]])'):format(subject, to, cc, bcc, attachments));
static int l_t1_load_mail_heads(lua_State* L)
{
    QString subject = QString::fromUtf8(lua_tolstring(L, 1, NULL));
    QString to = QString::fromUtf8(lua_tolstring(L, 2, NULL));
    QString cc = QString::fromUtf8(lua_tolstring(L, 3, NULL));
    QString bcc = QString::fromUtf8(lua_tolstring(L, 4, NULL));
    QString attachments = QString::fromUtf8(lua_tolstring(L, 5, NULL));

    that->load_mail_heads(subject, to, cc, bcc, attachments);
    return 0;
}

static int l_t1_set_variable(lua_State* L)
{
    QString name = QString::fromUtf8(lua_tolstring(L, 1, NULL));
    QString val = QString::fromUtf8(lua_tolstring(L, 2, NULL));
    that->setVariableLocked(name, val);
    return 0;
}

bool LuaExecuteThread::isQuit()
{
    return mQuit;
}

void LuaExecuteThread::run()
{
    L = luaL_newstate();             /* opens Lua */
    luaL_openlibs(L);        /* opens the standard libraries */

    lua_pushcfunction(L, l_selectArg);
    lua_setglobal(L, "select_args");

    lua_pushcfunction(L, l_selectApps);
    lua_setglobal(L, "select_apps");

    lua_pushcfunction(L, l_adb_push);
    lua_setglobal(L, "qt_adb_push");

    lua_pushcfunction(L, l_adb_pull);
    lua_setglobal(L, "qt_adb_pull");

    lua_pushcfunction(L, l_adb_install);
    lua_setglobal(L, "qt_adb_install");

    lua_pushcfunction(L, l_qt_adb_pipe);
    lua_setglobal(L, "qt_adb_pipe");

    lua_pushcfunction(L, l_logToUI);
    lua_setglobal(L, "log_to_ui");

    lua_pushcfunction(L, l_adbQuickInputAm);
    lua_setglobal(L, "adb_quick_input");

    lua_pushcfunction(L, l_adbQuickInputAm);
    lua_setglobal(L, "adb_quick_am");

    lua_pushcfunction(L, l_is_exiting);
    lua_setglobal(L, "is_exiting");

    lua_pushcfunction(L, l_t1_load_mail_heads);
    lua_setglobal(L, "t1_load_mail_heads");

    lua_pushcfunction(L, l_t1_set_variable);
    lua_setglobal(L, "t1_set_variable");

    int error = luaL_loadstring(L, "wrench = require('wrench')") || lua_pcall(L, 0, 0, 0);
    if (error) {
        emit gotSomeLog("exit", QString().sprintf("Can't load wrench: %s", lua_tolstring(L, -1, NULL)));
        lua_close(L);
        return;
    }

    lua_getglobal(L, "wrench");
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
            QString key = "exit";
            if (mQuit) {
                key = "quit";
            }
            emit gotSomeLog(key, QString().sprintf("Can't run %s: %s", qPrintable(func), lua_tolstring(L, -1, NULL)));
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
      mQuit(false),
      t1Sock(NULL)
{
}

LuaExecuteThread::~LuaExecuteThread()
{
    if (t1Sock)
        t1Sock->deleteLater();
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

void LuaExecuteThread::selectApps()
{
    emit selectAppsSig();
}

void LuaExecuteThread::on_argSelected(const QString& arg)
{
    mSelectArgsMutex.lock();
    mSelectedArg = arg;
    mSelectArgsMutex.unlock();
    mSelectArgsWait.wakeOne();
}

void LuaExecuteThread::load_mail_heads(const QString& subject, const QString& to, const QString& cc, const QString& bcc, const QString& attachments)
{
    emit load_mail_heads_sig(subject, to, cc, bcc, attachments);
}

void LuaExecuteThread::logToUI(const char *log)
{
    emit gotSomeLog("log", log);
}
