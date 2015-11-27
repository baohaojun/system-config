#include <libsnore/snore.h>
#include <libsnore/snore_p.h>
#include <libsnore/utils.h>

#include <QTextDocument>

#include <QtTest>

using namespace Snore;

class SnoreBenchmark : public QObject
{
    Q_OBJECT
public:
    SnoreBenchmark()
    {
        SnoreLog::setDebugLvl(3);
        SnoreCore &instance = SnoreCore::instance();
        instance.loadPlugins(SnorePlugin::BACKEND);
        instance.setSettingsValue(QStringLiteral("Timeout"), 1, LOCAL_SETTING);
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
    void benchmarkNotifications();

};

void SnoreBenchmark::benchmarkUtilsToHtml()
{

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
    QBENCHMARK {
        Utils::normalizeMarkup(htmlTestString, Utils::HREF);
    }
}

void SnoreBenchmark::benchmarkUtilsToHtmlAllMarkup()
{
    QCOMPARE(Utils::normalizeMarkup(htmlTestString, Utils::ALL_MARKUP), htmlTestString);

    QBENCHMARK {
        Utils::normalizeMarkup(htmlTestString, Utils::ALL_MARKUP);
    }
}

void SnoreBenchmark::benchmarkUtilsToPlain()
{
    QBENCHMARK {
        Utils::normalizeMarkup(htmlTestString, Utils::NO_MARKUP);
    }
}

void SnoreBenchmark::benchmarkNotifications()
{
    SnoreCore &instance = SnoreCore::instance();
    int closed = 0;
    connect(&instance, &SnoreCore::notificationClosed, [&closed](Notification) {
        closed++;
    });

    Application app = SnoreCorePrivate::instance()->defaultApplication();
    //   QBENCHMARK_ONCE{
    for (int i = 0; i < 100; ++i) {
        QString number = QString::number(i);
        Notification n(app, app.defaultAlert(), QLatin1String("Test ") + number, QLatin1String("Message ") + number, app.icon());
        instance.broadcastNotification(n);
    }
    // }
    while (closed < 100) {
        QTest::qWait(100);
    }

}

QTEST_MAIN(SnoreBenchmark)

#include "snorebenchmark.moc"
