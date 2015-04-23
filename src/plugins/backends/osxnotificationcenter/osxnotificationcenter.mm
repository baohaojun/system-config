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

OSXNotificationCenter::OSXNotificationCenter() : SnoreBackend("OSX Notification Center", false, false, false)
{
}

OSXNotificationCenter::~OSXNotificationCenter()
{
}

bool OSXNotificationCenter::initialize(SnoreCore *snore)
{
    notification_center = [NSUserNotificationCenter defaultUserNotificationCenter];

    return SnoreBackend::initialize(snore);
}

void OSXNotificationCenter::slotNotify(Snore::Notification notification)
{
    NSUserNotification *osx_notification = [[NSUserNotification alloc] init];
    osx_notification.title = NSStringFromQString(Utils::toPlainText(notification.title()));
    osx_notification.informativeText = NSStringFromQString(Utils::toPlainText(notification.text()));

    [notification_center deliverNotification: osx_notification];

    [osx_notification release];
}

