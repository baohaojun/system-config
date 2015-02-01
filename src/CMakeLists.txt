include_directories(${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/core ${CMAKE_CURRENT_BINARY_DIR}/core)

add_subdirectory(core)
add_subdirectory(daemon)
add_subdirectory(plugins)
