#include "readinfothread.hpp"
#include <QtCore/QString>
#include "bhj_help.hpp"
#include <QtCore/QStringList>
#include <QtCore/QDebug>
#include <QtCore/QProcess>

void ReadInfoThread::run()
{
    if (mProgressive) {
        return progressiveRun();
    }
    if (!mCommandList.isEmpty()) {
        QList<QStringList> savedCommands = mCommandList;
        mCommandList = QList<QStringList>();
        emit updateMobileInfo("start task", "");
        foreach(const QStringList& command, savedCommands) {
            mCommand = command.front();
            mArgs = command;
            mArgs.pop_front();
            emit updateMobileInfo(mReturnKey, "starting: " + mCommand + " " + mArgs.join(" "));
            run();
        }
        emit updateMobileInfo("end task", mReturnKey);
        return;
    }
    if (!mCommand.isEmpty()) {
        emit updateMobileInfo("start task", "");
        QString ret = mArgs.isEmpty()
            ? getExecutionOutput(mCommand)
            : getExecutionOutput(mCommand, mArgs);
        ret = ret.replace("\r", "");
        qDebug() << "ret is '" << ret << "'";

        if (mVerifyRet) {
            if (ret.right(strlen("ret: 0")) != "ret: 0") {
                if (ret.contains(QRegExp("ret: -?\\d+$"))) {
                    qDebug() << ret;
                    ret = "Command failed: " + ret.left(ret.indexOf(QRegExp("ret: -?\\d+$")));
                } else {
                    ret = "Error: return value not found";
                }
            } else {
                ret = ret.left(ret.length() - strlen("ret: 0"));
                while (ret.endsWith("\n") || ret.endsWith(" ") || ret.endsWith("\t")) {
                    ret.chop(1);
                }
            }
        }

        emit updateMobileInfo("end task", "");
        emit updateMobileInfo(mReturnKey, ret);
        return;
    }
}

ReadInfoThread::ReadInfoThread(QObject* parent)
    : QThread(parent)
{
    mProgressive = false;
}

void ReadInfoThread::setCommandArgsAndReturnKey(const QString& cmd, const QStringList& args, const QString& key, bool verifyRet)
{
    mCommand = cmd;
    mArgs = args;
    mReturnKey = key;
    mVerifyRet = verifyRet;
}

void ReadInfoThread::setCommandArgsAndReturnKey(const QString& cmd, const QString& key, bool verifyRet)
{
    setCommandArgsAndReturnKey(cmd, QStringList(), key);
    mVerifyRet = verifyRet;
}

void ReadInfoThread::setMultipleCommandsAndReturnKey(const QList<QStringList>& cmdAndArgs, const QString& key, bool verifyRet)
{
    mCommandList = cmdAndArgs;
    mReturnKey = key;
    mVerifyRet = verifyRet;
}

void ReadInfoThread::setProgressive()
{
    mProgressive = true;
}

void ReadInfoThread::progressiveRun()
{

    QProcess p;
    p.setProcessChannelMode(QProcess::MergedChannels);
    emit updateMobileInfo("start task", "");
    p.start(mCommand, mArgs);
    //p.start("bash", QStringList() << "-c" << "for x in $(seq 1 2000); do echo %Complete: $x; sleep .01; done");

    char buff[1024];

    QString outLine;
    p.closeWriteChannel();
    do {
        p.waitForReadyRead(-1);
        if (p.readLine(buff, sizeof(buff)) <= 0) {
            qDebug() << "Can't read for " << mReturnKey << " anymore";
            break;
        }
        outLine = buff;
        qDebug() << "outline: " << outLine;
        if (outLine.contains(QRegExp("%Complete|: FAIL|Error:"))) {
            emit updateMobileInfo(mReturnKey + " progress", outLine);
        }
    } while (1);

    if (!p.waitForFinished(-1)) {
        p.kill();
        p.waitForFinished();
    }

    emit updateMobileInfo(mReturnKey + " end", "");
    emit updateMobileInfo("end task", "");
}
