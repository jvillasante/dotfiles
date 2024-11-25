#
# Using FetchContent
#

# enable_testing()
# include(FetchContent)
# FetchContent_Declare(
#     googletest
#     GIT_REPOSITORY https://github.com/google/googletest.git
#     GIT_TAG        v1.15.x
# )

# set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
# FetchContent_MakeAvailable(googletest)

#
# Using vcpkg
#

enable_testing()
find_package(GTest CONFIG REQUIRED)
