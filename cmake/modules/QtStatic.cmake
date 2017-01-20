
if (SNORE_STATIC_QT)
    if (WIN32)
        if (CMAKE_BUILD_TYPE STREQUAL "Debug")
          set(d "d")
        else()
          set(d "")
        endif()

        if(MSVC)
            set(SUFFIX "lib")
            foreach(_bt DEBUG RELEASE RELWITHDEBINFO)
                string(REPLACE "/MD" "/MT" CMAKE_CXX_FLAGS_${_bt} ${CMAKE_CXX_FLAGS_${_bt}})
            endforeach(_bt DEBUG RELEASE RELWITHDEBINFO)
        else()
            set(SUFFIX "a")
        endif()


        link_libraries(
          "${_qt5Core_install_prefix}/lib/qtharfbuzzng${d}.${SUFFIX}"
          "${_qt5Core_install_prefix}/lib/qtpcre${d}.${SUFFIX}"
          #"${_qt5Core_install_prefix}/lib/qtpng${d}.${SUFFIX}"
          "${_qt5Core_install_prefix}/lib/qtfreetype${d}.${SUFFIX}"
          "${_qt5Core_install_prefix}/lib/Qt5PlatformSupport${d}.${SUFFIX}"
          "${_qt5Core_install_prefix}/plugins/platforms/qwindows${d}.${SUFFIX}"
          "${_qt5Core_install_prefix}/plugins/platforms/qwindows${d}.${SUFFIX}"
          "${_qt5Core_install_prefix}/plugins/mediaservice/dsengine${d}.${SUFFIX}"
          "Ws2_32"
          "Imm32"
          "Winmm"
          "Iphlpapi"
          "opengl32"
        )
        if (Qt5Quick_FOUND)
            link_libraries(
                "${_qt5Core_install_prefix}/qml/QtQuick.2/qtquick2plugin${d}.${SUFFIX}"
                "${_qt5Core_install_prefix}/qml/QtQuick/Window.2/windowplugin${d}.${SUFFIX}"
                )
        endif()
    endif()
endif()
