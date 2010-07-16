SET(LIBRARY_OUTPUT_PATH ${EXECUTABLE_OUTPUT_PATH}/snoreplugins)

add_subdirectory(freedesktopnotification)
add_subdirectory(freedesktopfrontend)
add_subdirectory(webposter)
add_subdirectory(snarlnetwork)
add_subdirectory(snarl)
#add_subdirectory(dbusbinding)
add_subdirectory(growl)

#find a clean solution for the webinterface
#add_subdirectory(redirector)
#add_subdirectory(registredapps)

