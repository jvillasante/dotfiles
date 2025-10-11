include(FetchContent)

Set(FETCHCONTENT_QUIET FALSE) # Needed to print downloading progress
FetchContent_Declare(
  Boost
  GIT_REPOSITORY https://github.com/boostorg/boost.git
  GIT_TAG boost-1.89.0
  GIT_SHALLOW TRUE
)

set(BOOST_INCLUDE_LIBRARIES thread)
set(BOOST_ENABLE_CMAKE ON)
FetchContent_MakeAvailable(Boost)

# add_executable(boost_test boost_test.cpp)
# target_link_libraries(boost_test PRIVATE Boost::filesystem
#                                          Boost::program_options)
