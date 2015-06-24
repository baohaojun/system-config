#include "osxnotificationcenter.h"
#include "libsnore/plugins/snorebackend.h"
#include "libsnore/utils.h"

#import <Foundation/Foundation.h>
#import <objc/runtime.h>

#import <QThread.h>
#import <QApplication.h>

using namespace Snore;

// store some variables that are needed (since obj-c++ does not allow having obj-c classes as c++ members)
namespace {
    NSUserNotificationCenter *notification_center;

    NSString *NSStringFromQString(QString qstr) {
        return [NSString stringWithUTF8String: qstr.toUtf8().data()];
    }
}


bool OSXNotificationCenter::initialize()
{
    notification_center = [NSUserNotificationCenter defaultUserNotificationCenter];

    return SnoreBackend::initialize();
}

void OSXNotificationCenter::slotNotify(Snore::Notification notification)
{
    NSUserNotification *osx_notification = [[NSUserNotification alloc] init];
    osx_notification.title = NSStringFromQString(Utils::toPlainText(notification.title()));
    osx_notification.informativeText = NSStringFromQString(Utils::toPlainText(notification.text()));
    slotNotificationDisplayed(notification);

    [notification_center deliverNotification: osx_notification];

    [osx_notification release];
}

