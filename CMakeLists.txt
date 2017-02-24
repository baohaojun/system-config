project(testproject)

# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
# Instruct CMake to run moc automatically when needed.
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)

# Find the QtWidgets library
find_package(Qt5Widgets)

# Tell CMake to create the helloworld executable
add_executable(helloworld main.cpp mainwindow.cpp mainwindow.ui)
# qt5_use_modules(helloworld Core Gui Widgets)

# Use the Widgets module from Qt 5.
target_link_libraries(helloworld Qt5::Widgets)
