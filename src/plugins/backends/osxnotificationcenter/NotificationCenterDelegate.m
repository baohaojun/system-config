#import "NotificationCenterDelegate.h"

@implementation NotificationCenterDelegate

- (BOOL)userNotificationCenter:(NSUserNotificationCenter *)center shouldPresentNotification:(NSUserNotification *)notification
{
	return YES;
}

@end
