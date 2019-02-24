cmake_minimum_required(VERSION 3.8)

# THIS HAS TO COME BEFORE THE PROJECT LINE
set(CLANG_PREFIX "/usr/local/opt/llvm)
set(CMAKE_C_COMPILER "${CLANG_PREFIX}/bin/clang")
set(CMAKE_CXX_COMPILER "${CLANG_PREFIX}/bin/clang++")
#
# set(GCC_PREFIX "/usr/local/Cellar/gcc/8.2.0")
# set(CMAKE_C_COMPILER "${GCC_PREFIX}/bin/gcc-8")
# set(CMAKE_CXX_COMPILER "${GCC_PREFIX}/bin/g++-8")
# THIS HAS TO COME BEFORE THE PROJECT LINE

# Specify default build type if none provided (before project() command)
if(NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build (Debug or Release)" FORCE)
endif()

# For generators with multiple configurations (like VS), only allow Debug and Release
if(CMAKE_CONFIGURATION_TYPES)
	set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Limited configurations" FORCE)
	mark_as_advanced(CMAKE_CONFIGURATION_TYPES)
endif()

# Project name
project(hands-on-system-programming-book VERSION 0.0.1 LANGUAGES C CXX)

# Compiler options
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS_COMMON "-std=c++17 -Wall -Wextra -Werror -march=native")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${CMAKE_CXX_FLAGS_COMMON} -DDEBUG -g -O2")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} ${CMAKE_CXX_FLAGS_COMMON} -O3")

# Mac OS X: clang uses libc++ standard library
if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-command-line-argument")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -nostdinc++")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -I${CLANG_PREFIX}/include")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -I${CLANG_PREFIX}/include/c++/v1")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -L${CLANG_PREFIX}/lib")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wl,-rpath,${CLANG_PREFIX}/lib")
  endif()
endif()

######################################################################################################################################

set(INCLUDE_PATH "${CMAKE_SOURCE_DIR}/include")
set(LIBS_PATH "/Users/jvillasante/Hacking/software")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/lib")

######################################################################################################################################
###### This is how to link with C++17 filesystem library
# if(CMAKE_COMPILER_IS_GNUCXX)
#   target_link_libraries(${PROJECT_NAME} stdc++fs)
# elseif(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
#   target_link_libraries(${PROJECT_NAME} c++fs)
# endif()
#
###### This is how to copy a file to bin
# add_custom_command(TARGET ${PROJECT_NAME} PRE_BUILD
#                    COMMAND ${CMAKE_COMMAND} -E copy
#                    ${PROJECT_SOURCE_DIR}/file.txt $<TARGET_FILE_DIR:${PROJECT_NAME}>)
#
# add_custom_command(TARGET ${PROJECT_NAME} PRE_BUILD
#                    COMMAND ${CMAKE_COMMAND} -E copy
#                    ${PROJECT_SOURCE_DIR}/file.txt ${CMAKE_CURRENT_BINARY_DIR})
#
###### This is how to include our own includes
# include_directories(${INCLUDE_PATH})
#
###### This is how to include directories of 3rd party libraries
# include_directories(${LIBS_PATH}/path/to/include)
#
######################################################################################################################################

add_subdirectory(src/app)
