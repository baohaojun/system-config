add_executable( snore-send main.cpp)
target_link_libraries( snore-send Snore::Libsnore)

install(TARGETS snore-send RUNTIME DESTINATION bin
                          LIBRARY DESTINATION lib
                          ARCHIVE DESTINATION lib)
