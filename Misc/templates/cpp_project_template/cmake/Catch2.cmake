Include(FetchContent)

Set(FETCHCONTENT_QUIET FALSE) # Needed to print downloading progress
FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG        v3.10.0 # or a later release
)

FetchContent_MakeAvailable(Catch2)

# Tell CMake where to find the Catch2 test discovery scripts
list(APPEND CMAKE_MODULE_PATH ${catch2_SOURCE_DIR}/extras)

# add_executable(tests test.cpp)
# target_link_libraries(tests PRIVATE Catch2::Catch2WithMain)
