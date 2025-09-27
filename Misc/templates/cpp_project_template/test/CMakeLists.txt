include(${PROJECT_SOURCE_DIR}/cmake/Catch2.cmake)

add_executable(libmath_tests libmath_tests.cpp)
target_link_libraries(libmath_tests
    PRIVATE math::libmath Catch2::Catch2WithMain)
add_test(libmath_tests ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/libmath_tests)
