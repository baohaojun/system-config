#include <libsnore/snore.h>
#include <libsnore/utils.h>

#include <QApplication>
#include <QDebug>

#include <QTextDocument>
#include <QTextDocumentFragment>

#include <QtTest>

using namespace Snore;


class SnoreBenchmark : public QObject{
    Q_OBJECT
public:
    SnoreBenchmark(){
        SnoreCore::instance();
    }


    QString htmlTestString = QLatin1String("<i>Italic A</i><br>"
                 "<i>Italic B</i><br>"
                 "<b>Bold</b><br>"
                 "<u>Underline</u><br>"
                 "<font color=\"blue\">Font</font><br>"
                 "&lt;&amp;&gt;<br>"
                 "<a href=\"https://github.com/Snorenotify/Snorenotify\">Website</a><br>");
private Q_SLOTS:
void benchmarkUtilsToHtml();
void benchmarkUtilsToHtmlAllMarkup();
void benchmarkUtilsToPlain();

};


void SnoreBenchmark::benchmarkUtilsToHtml(){


    QCOMPARE(Utils::normalizeMarkup(htmlTestString, Utils::NO_MARKUP), QLatin1String("Italic A\n"
                                                                               "Italic B\n"
                                                                               "Bold\n"
                                                                               "Underline\n"
                                                                               "Font\n"
                                                                               "<&>\n"
                                                                               "Website\n"));
    QCOMPARE(Utils::normalizeMarkup(htmlTestString, Utils::HREF), QLatin1String("Italic A\n"
                                                                          "Italic B\n"
                                                                          "Bold\n"
                                                                          "Underline\n"
                                                                          "Font\n"
                                                                          "&lt;&amp;&gt;\n"
                                                                          "<a href=\"https://github.com/Snorenotify/Snorenotify\">Website</a>\n"));
    QCOMPARE(Utils::normalizeMarkup(htmlTestString, Utils::HREF | Utils::BOLD | Utils::BREAK |
                                    Utils::UNDERLINE | Utils::FONT | Utils::ITALIC), htmlTestString);
    QBENCHMARK{
        Utils::normalizeMarkup(htmlTestString, Utils::HREF);
    }
}

void SnoreBenchmark::benchmarkUtilsToHtmlAllMarkup()
{
    QCOMPARE(Utils::normalizeMarkup(htmlTestString, Utils::ALL_MARKUP), htmlTestString);

    QBENCHMARK{
        Utils::normalizeMarkup(htmlTestString, Utils::ALL_MARKUP);
    }
}

void SnoreBenchmark::benchmarkUtilsToPlain()
{
    QBENCHMARK{
        Utils::normalizeMarkup(htmlTestString, Utils::NO_MARKUP);
    }
}
QTEST_MAIN(SnoreBenchmark)

#include "snorebenchmark.moc"
