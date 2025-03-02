#
# Using FetchContent
#

# Disable the Google Benchmark requirement on Google Test
set(BENCHMARK_ENABLE_TESTING NO)
set(BENCHMARK_ENABLE_GTEST_TESTS NO)

include(FetchContent)
FetchContent_Declare(
    googlebenchmark
    GIT_REPOSITORY https://github.com/google/benchmark.git
    GIT_TAG        v1.9.0
)

FetchContent_MakeAvailable(googlebenchmark)

#
# Using vcpkg
#
# find_package(benchmark CONFIG REQUIRED)
