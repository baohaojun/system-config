#include "bhj_help.hpp"
#include <QMap>
#include <QFile>
#include <QTextStream>
#include <QRegularExpression>
#include <QRegularExpressionMatch>
#include <QDebug>

bool matchOneString(const QStringList& strList, const QString& str)
{
    foreach(const QString& entry, strList) {
        if (entry.contains(str, Qt::CaseInsensitive)) {
            return true;
        }
    }
    return false;
}

QStringList filterMatchedStrings(const QStringList& strList, const QString& str)
{
    QStringList res;
    foreach(const QString& entry, strList) {
        if (entry.contains(str, Qt::CaseInsensitive)) {
            res << entry;
        }
    }
    return res;
}

QStringList getPinyinSpelling(const QString& str)
{
    static QMap<QString, QStringList> pinYinMap;

    if (pinYinMap.isEmpty()) {
        QFile py("uc-to-py.tbl");
        if (!py.open(QIODevice::ReadOnly | QIODevice::Text)) {
            return QStringList(str);
        }
        QTextStream in(&py);
        QRegularExpression re("^([0-9a-f]{4})\\s+\\((.*?)\\)\\s*$", QRegularExpression::CaseInsensitiveOption);
        QRegularExpressionMatch match;
        QRegularExpression splitRe(":|[0-9]|,");
        while (!in.atEnd()) {
            QString line = in.readLine();
            if (line.contains(re, &match)) {
                QString chr = match.captured(1);
                QString pinyinStr = match.captured(2);

                QStringList pinyinList  = pinyinStr.split(splitRe, QString::SkipEmptyParts);

                QChar ch = chr.toInt(0, 16);
                pinYinMap[ch] = pinyinList;
            }
        }
    }

    QStringList res("");
    foreach(const QChar& ch, str) {
        QStringList pyList;
        if (pinYinMap.contains(QString(ch))) {
            pyList = pinYinMap[QString(ch)];
        } else {
            pyList << ch;
        }
        QStringList midRes;
        foreach(const QString& pre, res) {
            foreach(const QString& post, pyList) {
                midRes << (pre + post);
            }
        }
        res = midRes;
    }
    return res;
}
