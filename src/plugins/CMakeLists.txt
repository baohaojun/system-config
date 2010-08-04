SET(LIBRARY_OUTPUT_PATH ${EXECUTABLE_OUTPUT_PATH}/snoreplugins)

if(WITH_FREEDESKTOP_FRONTEND)
    add_subdirectory(freedesktopnotification)
else(WITH_FREEDESKTOP_FRONTEND)
    add_subdirectory(freedesktopfrontend)
endif(WITH_FREEDESKTOP_FRONTEND)
#add_subdirectory(webposter)
add_subdirectory(snarlnetwork)
add_subdirectory(snarl)
#add_subdirectory(dbusbinding)
add_subdirectory(growl)

#find a clean solution for the webinterface
#add_subdirectory(redirector)
#add_subdirectory(registredapps)

