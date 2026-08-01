include(${PROJECT_SOURCE_DIR}/cmake/Catch2.cmake)

add_executable(libmath_tests libmath_tests.cpp)

target_link_libraries(libmath_tests
    PRIVATE
        math::libmath
        Catch2::Catch2WithMain
)

set_target_properties(libmath_tests PROPERTIES
    CXX_STANDARD 23
    CXX_STANDARD_REQUIRED ON
    CXX_EXTENSIONS OFF
)

target_compile_options(libmath_tests
    PRIVATE
        -Wno-c2y-extensions
)

# Let Catch2 register each TEST_CASE individually
include(Catch)
catch_discover_tests(libmath_tests)
