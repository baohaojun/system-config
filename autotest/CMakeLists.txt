include(ECMAddTests)
find_package(Qt5Test)

ecm_add_tests( snorebenchmark.cpp LINK_LIBRARIES Snore::Libsnore Qt5::Test)
