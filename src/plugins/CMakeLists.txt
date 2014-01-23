set(CMAKE_SHARED_MODULE_PREFIX)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib/libsnore)

add_subdirectory(backends)
add_subdirectory(frontends)
add_subdirectory(secondary_backends)
add_subdirectory(plugins)
