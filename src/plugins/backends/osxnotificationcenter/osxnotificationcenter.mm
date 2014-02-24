#include "osxnotificationcenter.h"
#include "core/plugins/snorebackend.h"

#import <Foundation/NSUserNotification.h>
//#import "NotificationCenterDelegate.h"

Q_EXPORT_PLUGIN2(libsnore_backend_osxnotificationcenter, OSXNotificationCenter)

using namespace Snore;

namespace {
	NSUserNotificationCenter *notification_center;
	//NotificationCenterDelegate *notification_center_delegate;
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
	//notification_center_delegate = [[NotificationCenterDelegate alloc] init];
	//[notification_center setDelegate:notification_center_delegate];

	return SnoreBackend::initialize(snore);
}

void OSXNotificationCenter::slotNotify(Snore::Notification notification)
{
	NSUserNotification *osx_notification = [[NSUserNotification alloc] init];
	osx_notification.title = @"Test";
	osx_notification.informativeText = @"Hello";

	[notification_center deliverNotification: osx_notification];

	[osx_notification release];
}

