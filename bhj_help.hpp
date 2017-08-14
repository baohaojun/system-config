#ifndef _BHJ_HELP_H_
#define _BHJ_HELP_H_
#include <QtCore/QString>
#include <QStringList>

QString getExecutionOutput(const QString& cmd, const QStringList& args);
QString getExecutionOutput(const QString& cmd);
QString fixPathName(const QString& path);
bool    matchOneString(const QStringList& strList, const QString& str);
QStringList filterMatchedStrings(const QStringList& strList, const QString& str);
QStringList getPinyinSpelling(const QString& str, bool only_first_spell = 0);
QString shell_quote(const QString& str);
QStringList shell_quote(const QStringList& args);
bool isWrenchQuitting();
void wrenchSetQuitting();
#endif /* _BHJ_HELP_H_ */
