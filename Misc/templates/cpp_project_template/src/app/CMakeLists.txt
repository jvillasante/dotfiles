set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin")
include_directories(${INCLUDE_PATH})
find_package(GTest CONFIG REQUIRED)
find_package(benchmark CONFIG REQUIRED)

set(PROGRAM_NAME tests)
add_executable(${PROGRAM_NAME} ${PROGRAM_NAME}.cpp)
target_link_libraries(${PROGRAM_NAME} PRIVATE
    GTest::gtest GTest::gtest_main GTest::gmock GTest::gmock_main)
add_test(${PROGRAM_NAME} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${PROGRAM_NAME})

set(PROGRAM_NAME bench)
add_executable(${PROGRAM_NAME} ${PROGRAM_NAME}.cpp)
target_link_libraries(${PROGRAM_NAME} PRIVATE
    benchmark::benchmark benchmark::benchmark_main)
