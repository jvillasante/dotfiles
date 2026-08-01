cmake_minimum_required(VERSION 3.27)

# Initialize the project and specify languages
project(
  project-name
  VERSION 0.0.1
  DESCRIPTION ""
  LANGUAGES CXX
)

# Single source of truth for the language standard. Applied as the default for
# every target created after this point (lib, tools, tests).
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Setup ccache
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
    set(CMAKE_CXX_COMPILER_LAUNCHER ${CCACHE_PROGRAM})
    message(STATUS "Ccache found: ${CCACHE_PROGRAM}")
else()
    message(STATUS "Ccache not found")
endif()

# CTest automatically calls enable_testing() and provides BUILD_TESTING
include(CTest)

# Personal C++ Utils
include(${PROJECT_SOURCE_DIR}/cmake/CppUtils.cmake)

# Sources
add_subdirectory("src")
add_subdirectory("tools")

# Conditionally include the test directory
if(BUILD_TESTING)
    add_subdirectory("test")
endif()
