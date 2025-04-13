#
# Using FetchContent
#

include(FetchContent)
FetchContent_Declare(
    cpp_utils
    GIT_REPOSITORY git@github.com:jvillasante/cpp_utils.git
    GIT_TAG        origin/main
)

FetchContent_MakeAvailable(cpp_utils)

# Include header only lib here
include_directories("${FETCHCONTENT_BASE_DIR}/cpp_utils-src/include")
