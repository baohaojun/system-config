#include "luaexecutethread.hpp"
#include <QtCore/QString>
#include "bhj_help.hpp"
#include <QtCore/QStringList>
#include <QtCore/QDebug>
#include <QtCore/QProcess>
#include "lua.hpp"
#include <QtWidgets/QMessageBox>
#include <QTcpSocket>
#include "t1wrench.h"

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

void LuaExecuteThread::t1SockStateChange(QAbstractSocket::SocketState newState)
{
    qDebug() << "state is " << newState;
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
        t1Sock = new QTcpSocket(this);
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
            t1Sock->waitForReadyRead();
            res = t1Sock->readLine();
            qDebug() << "got result" << res;
        }
    } else if (arg.startsWith("am ")) {
        t1Sock->write(arg.toUtf8() + "\n");
        t1Sock->flush();
        t1Sock->waitForReadyRead();
        res = t1Sock->readLine();
        qDebug() << "got result" << res;
    } else {
        QString error = "Invalid t1sock input: " + arg;
        luaL_argerror(L, 1, qPrintable(error));
    }

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

void LuaExecuteThread::run()
{
    L = luaL_newstate();             /* opens Lua */
    luaL_openlibs(L);        /* opens the standard libraries */

    lua_pushcfunction(L, l_selectArg);
    lua_setglobal(L, "select_args");

    lua_pushcfunction(L, l_adbQuickInputAm);
    lua_setglobal(L, "adb_quick_input");

    lua_pushcfunction(L, l_adbQuickInputAm);
    lua_setglobal(L, "adb_quick_am");

    lua_pushcfunction(L, l_t1_load_mail_heads);
    lua_setglobal(L, "t1_load_mail_heads");

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
      mQuit(false),
      t1Sock(NULL)
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

void LuaExecuteThread::load_mail_heads(const QString& subject, const QString& to, const QString& cc, const QString& bcc, const QString& attachments)
{
    emit load_mail_heads_sig(subject, to, cc, bcc, attachments);
}
