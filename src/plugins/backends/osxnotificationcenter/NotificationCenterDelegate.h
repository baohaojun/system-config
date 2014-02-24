#import <Foundation/Foundation.h>
#import <Foundation/NSUserNotification.h>

@interface NotificationCenterDelegate : NSObject <NSUserNotificationCenterDelegate> {
	// no instance variables
}
- (BOOL)userNotificationCenter:(NSUserNotificationCenter *)center shouldPresentNotification:(NSUserNotification *)notification;
@end
