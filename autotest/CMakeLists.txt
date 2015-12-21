include(ECMAddTests)
find_package(Qt5Test QUIET)

set_package_properties(Qt5Test PROPERTIES
    DESCRIPTION "Qt5 unit testing framework"
    URL "http://www.qt.io"
    TYPE OPTIONAL
    PURPOSE "Required to build the unit tests"
)

if(NOT Qt5Test_FOUND)
    return()
endif()

ecm_add_tests( snorebenchmark.cpp display_test.cpp LINK_LIBRARIES Snore::Libsnore Qt5::Test)
