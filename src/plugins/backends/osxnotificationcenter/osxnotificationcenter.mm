#include "osxnotificationcenter.h"
#include "core/plugins/snorebackend.h"

#import <Foundation/Foundation.h>
#import <objc/runtime.h>

#import <QThread.h>
#import <QApplication.h>

Q_EXPORT_PLUGIN2(libsnore_backend_osxnotificationcenter, OSXNotificationCenter)

using namespace Snore;

// store some variables that are needed (since obj-c++ does not allow having obj-c classes as c++ members)
namespace {
    NSUserNotificationCenter *notification_center;
    NSString *appID = @"dummyID.snorenotify";

    // make sure __bundleIdentifier is called on every NSBundle object instead of bundleIdentifier
    // see http://stackoverflow.com/a/14698543
    bool installNSBundleHook() {
        Class cls = objc_getClass("NSBundle");
        if (cls) {
            method_exchangeImplementations(class_getInstanceMethod(cls, @selector(bundleIdentifier)),
                                           class_getInstanceMethod(cls, @selector(__bundleIdentifier)));
            return true;
        }
        return false;
    }

    NSString *NSStringFromQString(QString qstr) {
        return [NSString stringWithUTF8String: qstr.toUtf8().data()];
    }
}

// this category adds the __bundleIdentifier method to every NSBundle object
// see http://stackoverflow.com/a/14698543
@implementation NSBundle(snore)
- (NSString *)__bundleIdentifier {
    return appID;
}
@end


OSXNotificationCenter::OSXNotificationCenter() : SnoreBackend("OSX Notification Center", false, false, false)
{
}

OSXNotificationCenter::~OSXNotificationCenter()
{
}

bool OSXNotificationCenter::initialize(SnoreCore *snore)
{
    installNSBundleHook();
    notification_center = [NSUserNotificationCenter defaultUserNotificationCenter];

    return SnoreBackend::initialize(snore);
}

void OSXNotificationCenter::slotNotify(Snore::Notification notification)
{
    QString qAppID = QString("%1.%2").arg(qApp->organizationName(), qApp->applicationName()).remove(" ");
    appID = NSStringFromQString(qAppID);

    snoreDebug(SNORE_DEBUG) << "bundle identifier for notification:" << [[[NSBundle mainBundle] bundleIdentifier] UTF8String];


    NSUserNotification *osx_notification = [[NSUserNotification alloc] init];
    osx_notification.title = NSStringFromQString(Snore::toPlainText(notification.title()));
    osx_notification.informativeText = NSStringFromQString(Snore::toPlainText(notification.text()));

    [notification_center deliverNotification: osx_notification];

    [osx_notification release];
}

