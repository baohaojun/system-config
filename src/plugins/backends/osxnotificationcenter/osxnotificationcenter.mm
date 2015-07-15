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

QMap<int, Notification> m_IdToNotification;
NSMutableDictionary * m_IdToNSNotification;


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
    int notificationId = [notification.userInfo[@"id"] intValue];
    [center removeDeliveredNotification: notification];
    if (not m_IdToNotification.contains(notificationId)) {
        snoreDebug(SNORE_WARNING) << "User clicked on notification that was not recognized";
        return;
    }
    auto snoreNotification = m_IdToNotification.take(notificationId);
    snoreNotification.data()->setCloseReason(Notification::ACTIVATED);
    emitNotificationClicked(snoreNotification);
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
        return [NSString stringWithUTF8String: qstr.toUtf8().constData()];
    }
}

UserNotificationItemClass * delegate;

OSXNotificationCenter::OSXNotificationCenter()
{
    m_IdToNSNotification = [[[NSMutableDictionary alloc] init] autorelease];
    delegate = new UserNotificationItemClass();
}

OSXNotificationCenter::~OSXNotificationCenter()
{
    delete delegate;
}

void OSXNotificationCenter::slotNotify(Snore::Notification notification)
{
    NSUserNotification * osxNotification = [[[NSUserNotification alloc] init] autorelease];
    NSString * notificationId = [NSString stringWithFormat:@"%d",notification.id()];
    osxNotification.title = NSStringFromQString(notification.title());
    osxNotification.userInfo = [NSDictionary dictionaryWithObjectsAndKeys:notificationId, @"id", nil];
    osxNotification.informativeText = NSStringFromQString(notification.text());
    
    // Add notification to mapper from id to Nofification / NSUserNotification
    m_IdToNotification.insert(notification.id(), notification);
    [m_IdToNSNotification setObject:osxNotification forKey: notificationId];
    [[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification: osxNotification];
    
    slotNotificationDisplayed(notification);
}

void OSXNotificationCenter::load()
{
	emit loadedStateChanged(true);
}


