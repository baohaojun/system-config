set(SnoreNotify_SRCS ${SnoreNotify_SRCS}
        notification/notification.cpp
        notification/notification_p.cpp
        notification/notificationaction.cpp
        notification/icon.cpp
        PARENT_SCOPE)

set(SnoreNotify_HDR
        notification.h
        notificationaction.h
        icon.h
        )

install(FILES ${SnoreNotify_HDR} DESTINATION ${LIBSNORE_INCLUDE_DIR}/notification)
