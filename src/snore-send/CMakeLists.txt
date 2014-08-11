if(WITH_SNORE_SEND)
    if(NOT WITH_QT4)
        message(STATUS "Building snore-send")
        add_executable( snore-send main.cpp ${SNORENOTIFY_DEAMON_DEPS})
        target_link_libraries( snore-send libsnore Qt5::Core )

        if(KDE5_FOUND)
                target_link_libraries( snore-send KF5::CoreAddons KF5::I18n)
        endif()

        install(TARGETS snore-send RUNTIME DESTINATION bin
                                  LIBRARY DESTINATION lib
                                  ARCHIVE DESTINATION lib)
    else()
        message(STATUS "Don't build snore-send, as it depends on Qt5")
    endif()
endif()
