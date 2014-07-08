#ifndef _BHJ_HELP_H_
#define _BHJ_HELP_H_
#include <QtCore/QString>

QString getExecutionOutput(const QString& cmd, const QStringList& args);
QString getExecutionOutput(const QString& cmd);
QString getStcmdCmdResult(const QString& cmd, bool stripRetCode = true);
QString fixPathName(const QString& path);
QString quoteCmdArg(const QString& arg);

#endif /* _BHJ_HELP_H_ */
