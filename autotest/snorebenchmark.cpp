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


    QString htmlTestString = QString("<i>Italic A</i><br>"
                 "<i>Italic B</i><br>"
                 "<b>Bold</b><br>"
                 "<u>Underline</u><br>"
                 "<font color=\"blue\">Font</font><br>"
                 "&lt;&amp;&gt;"
                 "<a href=\"https://github.com/Snorenotify/Snorenotify\">Website</a><br>");
private slots:
void benchmarkUtilsToHtml();
void benchmarkUtilsToHtmlAllMarkup();
void benchmarkUtilsToPlain();

private:

/**
 * old toPlaintext function from Utils.
 * @param string A string to decode if needed.
 * @return if the string was rhichtext or html encoded a decoded string, else the original string.
 */
static inline QString toPlainText(const QString &string)
{
    if (Qt::mightBeRichText(string)) {
        return QTextDocumentFragment::fromHtml(string).toPlainText();
    } else {
        return string;
    }
}
};


void SnoreBenchmark::benchmarkUtilsToHtml(){


    QCOMPARE(Utils::normaliseMarkup(htmlTestString, Utils::NO_MARKUP), toPlainText(htmlTestString));
    QCOMPARE(Utils::normaliseMarkup(htmlTestString, Utils::HREF), QString("Italic A\n"
                                                                          "Italic B\n"
                                                                          "Bold\n"
                                                                          "Underline\n"
                                                                          "Font\n"
                                                                          "&lt;&amp;&gt;"
                                                                          "<a href=\"https://github.com/Snorenotify/Snorenotify\">Website</a>\n"));
    QCOMPARE(Utils::normaliseMarkup(htmlTestString, Utils::HREF | Utils::BOLD | Utils::BREAK |
                                    Utils::UNDERLINE | Utils::FONT | Utils::ITALIC), htmlTestString);
    QBENCHMARK{
        Utils::normaliseMarkup(htmlTestString, Utils::HREF);
    }
}

void SnoreBenchmark::benchmarkUtilsToHtmlAllMarkup()
{
    QCOMPARE(Utils::normaliseMarkup(htmlTestString, Utils::ALL_MARKUP), htmlTestString);

    QBENCHMARK{
        Utils::normaliseMarkup(htmlTestString, Utils::ALL_MARKUP);
    }
}

void SnoreBenchmark::benchmarkUtilsToPlain()
{
    QBENCHMARK{
        Utils::normaliseMarkup(htmlTestString, Utils::NO_MARKUP);
    }
}
QTEST_MAIN(SnoreBenchmark)

#include "snorebenchmark.moc"
