#include <libsnore/snore.h>
#include <libsnore/snore_p.h>
#include <libsnore/utils.h>

#include <QTextDocument>

#include <QtTest>

using namespace Snore;

class DisplayTest : public QObject
{
    Q_OBJECT
public:
    DisplayTest():
        app(QLatin1String("Test"), Icon::defaultIcon())
    {
        SnoreLog::setDebugLvl(3);
        SnoreCore &instance = SnoreCore::instance();
        instance.loadPlugins(SnorePlugin::BACKEND);
        instance.setSettingsValue(QLatin1String("Timeout"), 5, LOCAL_SETTING);
        SnoreCore::instance().registerApplication(app);
    }

    Application app;

private Q_SLOTS:
    void displayTest();
    void displayTestPlani();

private:
    void testString(QString message){
        qDebug() << Utils::normalizeMarkup(message, Utils::NO_MARKUP);
        SnoreCore &snore = SnoreCore::instance();
        QStringList backends = snore.pluginNames(SnorePlugin::BACKEND);
        auto notify = [&backends, &snore, &message, this](Notification n){
            qDebug() << n << "closed";
            qDebug() << backends.size();
            if(backends.empty()) {
                return;
            }
            QString old = snore.primaryNotificationBackend();
            while(snore.primaryNotificationBackend() == old) {
                QString p = backends.takeLast();
                snore.setSettingsValue(QLatin1String("PrimaryBackend"), p, LOCAL_SETTING);
                SnoreCorePrivate::instance()->syncSettings();
                if (snore.primaryNotificationBackend() == p) {
                    qDebug() << p;
                    snore.broadcastNotification(Notification(app, app.defaultAlert(), QLatin1String("Title"), message, app.icon()));
                }
            }
        };
        auto con = connect(&snore, &SnoreCore::notificationClosed, notify);
        notify(Notification());
        while (!backends.empty()) {
            QTest::qWait(100);
        }
        QTest::qWait(100);
        disconnect(con);
    }
};


void DisplayTest::displayTest()
{
    app.hints().setValue("use-markup", true);
    testString(QLatin1String("<b>Test&#937;</b>&#x1f4a9;&#x1f600;"));
}


void DisplayTest::displayTestPlani()
{
    app.hints().setValue("use-markup", false);
    testString(QString::fromUtf8("Test\u03A9\U0001F4A9\U0001F600"));
}


QTEST_MAIN(DisplayTest)

#include "display_test.moc"
