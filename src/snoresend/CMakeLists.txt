add_executable( snore-send main.cpp)
target_link_libraries( snore-send Snore::Libsnore)

install(TARGETS snore-send ${KDE_INSTALL_TARGETS_DEFAULT_ARGS})
