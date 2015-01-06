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
        if (entry.indexOf(str) >= 0) {
            return true;
        }
    }
    return false;
}

QStringList filterMatchedStrings(const QStringList& strList, const QString& str)
{
    QStringList res;
    foreach(const QString& entry, strList) {
        if (entry.indexOf(str) >= 0) {
            res << entry;
        }
    }
    return res;
}

QStringList getPinyinSpelling(const QString& str)
{
    qDebug() << "get spell for " << str;
    static QMap<QString, QStringList> pinYinMap;

    if (pinYinMap.isEmpty()) {
        QFile py("uc-to-py.tbl");
        if (!py.open(QIODevice::ReadOnly | QIODevice::Text)) {
            qDebug() << "Can't open uc-to-py.tbl";
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
                if (chr.toInt(0, 16) % 1000 == 0) {
                    qDebug() << "ch is " << ch;
                }
                pinYinMap[ch] = pinyinList;
            }
        }
    }

    QStringList res("");
    foreach(const QChar& ch, str) {
        qDebug() << "ch.2 is " << ch;
        QStringList pyList;
        if (pinYinMap.contains(QString(ch))) {
            qDebug() << "yes " << ch << " is there";
            pyList = pinYinMap[QString(ch)];
        } else {
            pyList << ch;
        }
        QStringList midRes;
        foreach(const QString& pre, res) {
            foreach(const QString& post, pyList) {
                qDebug() << "pre is " << pre << ", post is " << post;
                midRes << (pre + post);
            }
        }
        res = midRes;
    }
    qDebug() << "return is " << res;
    return res;
}
