set ( SnoreNotify_SRCS ${SnoreNotify_SRCS}
        notification/notification.cpp
        notification/notification_p.cpp
        notification/notificationaction.cpp
        notification/icon.cpp
        notification/icon_p.cpp
        PARENT_SCOPE)

set ( Notification_HDR 
        notification.h
        notification_p.h
        notificationaction.h
        icon.h
        notificationenums.h
        )

install(FILES ${Notification_HDR} DESTINATION include/snore/core/notification)




