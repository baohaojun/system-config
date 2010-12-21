set(SNORE_RCS ${SNORE_RCS} ../data/snore.qrc PARENT_SCOPE)

if(WIN32)
  file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/zzz.rc "IDI_ICON1        ICON        DISCARDABLE    \"zzz.ico\"\n")
  if(MINGW)
    find_program(WINDRES_EXECUTABLE NAMES windres)
    if(WINDRES_EXECUTABLE)
      exec_program(windres
              ARGS "-i ${CMAKE_CURRENT_BINARY_DIR}/zzz.rc"
                   "-o ${CMAKE_CURRENT_BINARY_DIR}/zzz.o"
                   "--include-dir=${CMAKE_CURRENT_SOURCE_DIR}")
      set(SNORENOTIFY_DEPS ${SNORENOTIFY_DEPS} ${CMAKE_CURRENT_BINARY_DIR}/zzz.o PARENT_SCOPE)
    endif(WINDRES_EXECUTABLE)
  else(MINGW)
    set(SNORENOTIFY_DEPS ${SNORENOTIFY_DEPS} ../data/zzz.rc PARENT_SCOPE)
  endif(MINGW)
endif(WIN32)