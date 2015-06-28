#include "osxnotificationcenter.h"
#include "libsnore/plugins/snorebackend.h"
#include "libsnore/notification/notification_p.h"
#include "libsnore/utils.h"
#include "libsnore/snore.h"
#include "libsnore/log.h"

#import <QThread.h>
#import <QApplication.h>
#import <QMap>
#include <Foundation/Foundation.h>

using namespace Snore;

QMap<int, Notification> id_to_notification;
NSMutableDictionary * id_to_nsnotification;


void emitNotificationClicked(Notification notification) {
    emit SnoreCore::instance().actionInvoked(notification);
}

//
// Enable reaction when user clicks on NSUserNotification
//

@interface UserNotificationItem : NSObject<NSUserNotificationCenterDelegate> { }

- (BOOL)userNotificationCenter:(NSUserNotificationCenter *)center shouldPresentNotification:(NSUserNotification *)notification;
@end

@implementation UserNotificationItem
- (BOOL)userNotificationCenter:(NSUserNotificationCenter *)center shouldPresentNotification:(NSUserNotification *)notification {
    Q_UNUSED(center);
    Q_UNUSED(notification);
    return YES;
}
- (void) userNotificationCenter:(NSUserNotificationCenter *)center didActivateNotification:(NSUserNotification *)notification
{
    
    snoreDebug(SNORE_DEBUG) << "User clicked on notification";
    int notification_id = [notification.userInfo[@"id"] intValue];
    [center removeDeliveredNotification: notification];
    if (not id_to_notification.contains(notification_id)) {
        snoreDebug(SNORE_WARNING) << "User clicked on notification that was not recognized";
        return;
    }
    Notification snore_notification = id_to_notification[notification_id];
    snore_notification.data()->setCloseReason(Notification::ACTIVATED);
    emitNotificationClicked(snore_notification);
}
@end

class UserNotificationItemClass
{
public:
    UserNotificationItemClass() {
        item = [UserNotificationItem alloc];
        if (QSysInfo::MacintoshVersion >= QSysInfo::MV_10_8) {
            [[NSUserNotificationCenter defaultUserNotificationCenter] setDelegate:item];
        }
    }
    ~UserNotificationItemClass() {
        if (QSysInfo::MacintoshVersion >= QSysInfo::MV_10_8) {
            [[NSUserNotificationCenter defaultUserNotificationCenter] setDelegate:nil];
        }
        [item release];
    }
    UserNotificationItem *item;
};


// store some variables that are needed (since obj-c++ does not allow having obj-c classes as c++ members)
namespace {
    
    NSString * NSStringFromQString(QString qstr) {
        return [NSString stringWithUTF8String: qstr.toUtf8().data()];
    }
}

UserNotificationItemClass * delegate;

OSXNotificationCenter::OSXNotificationCenter()
{
    id_to_nsnotification = [[[NSMutableDictionary alloc] init] autorelease];
    delegate = new UserNotificationItemClass();
}

OSXNotificationCenter::~OSXNotificationCenter()
{
    delete delegate;
}

bool OSXNotificationCenter::initialize()
{
    return SnoreBackend::initialize();
}

void OSXNotificationCenter::slotNotify(Snore::Notification notification)
{
    NSUserNotification * osx_notification = [[[NSUserNotification alloc] init] autorelease];
    NSString * notification_id = [NSString stringWithFormat:@"%d",notification.id()];
    osx_notification.title = NSStringFromQString(notification.title());
    osx_notification.userInfo = [NSDictionary dictionaryWithObjectsAndKeys:notification_id, @"id", nil];
    osx_notification.informativeText = NSStringFromQString(notification.text());
    
    // Add notification to mapper from id to Nofification / NSUserNotification
    id_to_notification.insert(notification.id(), notification);
    [id_to_nsnotification setObject:osx_notification forKey: notification_id];
    [[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification: osx_notification];
    
    slotNotificationDisplayed(notification);
}

