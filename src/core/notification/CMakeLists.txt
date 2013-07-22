set ( SnoreNotify_SRCS ${SnoreNotify_SRCS}
        notification/notification.cpp
        notification/icon.cpp
        PARENT_SCOPE)

set ( Notification_HDR 
        notification.h
        icon.h
        notificationenums.h
        )

install(FILES ${Notification_HDR} DESTINATION include/snore/core/notification)




