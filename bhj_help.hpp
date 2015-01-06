#ifndef _BHJ_HELP_H_
#define _BHJ_HELP_H_
#include <QtCore/QString>
#include <QStringList>

QString getExecutionOutput(const QString& cmd, const QStringList& args);
QString getExecutionOutput(const QString& cmd);
QString getStcmdCmdResult(const QString& cmd, bool stripRetCode = true);
QString fixPathName(const QString& path);
bool    matchOneString(const QStringList& strList, const QString& str);
QStringList filterMatchedStrings(const QStringList& strList, const QString& str);
QStringList getPinyinSpelling(const QString& str);

#endif /* _BHJ_HELP_H_ */
