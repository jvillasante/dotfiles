cmake_minimum_required(VERSION 3.15)

# THIS HAS TO COME BEFORE THE PROJECT LINE
set(CLANG_PREFIX "/usr/local/opt/llvm")
set(CMAKE_C_COMPILER "${CLANG_PREFIX}/bin/clang")
set(CMAKE_CXX_COMPILER "${CLANG_PREFIX}/bin/clang++")
# THIS HAS TO COME BEFORE THE PROJECT LINE

# Specify default build type if none provided (before project() command)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE
      Release
      CACHE STRING "Choose the type of build (Debug or Release)" FORCE)
endif()

# For generators with multiple configurations (like VS), only allow Debug and
# Release
if(CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_CONFIGURATION_TYPES
      "Debug;Release"
      CACHE STRING "Limited configurations" FORCE)
  mark_as_advanced(CMAKE_CONFIGURATION_TYPES)
endif()

# Project name
project(
  sfml-project
  VERSION 0.0.1
  LANGUAGES C CXX)

# Configuration options
set(SFML_STATIC_LIBRARIES
    FALSE
    CACHE BOOL "Use static SFML librares")

# General compiler options
if(SFML_STATIC_LIBRARIES)
  add_definitions(-DSFML_STATIC)
endif()

# Compiler options
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_FLAGS_COMMON "-std=c++20 -Wall -Wextra -Werror")
set(CMAKE_CXX_FLAGS_DEBUG
    "${CMAKE_CXX_FLAGS_DEBUG} ${CMAKE_CXX_FLAGS_COMMON} -DDEBUG -g -O0")
set(CMAKE_CXX_FLAGS_RELEASE
    "${CMAKE_CXX_FLAGS_RELEASE} ${CMAKE_CXX_FLAGS_COMMON} -O3")

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

# Add directory containing FindSFML.cmake to module path
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMake/;${CMAKE_MODULE_PATH}")

# Find SFML Note: SFML_STATIC_LIBRARIES determines which libraries are found by
# find_package()
find_package(SFML 2 COMPONENTS audio graphics window system network)

# If found, include and link; otherwise output error message
if(SFML_FOUND)
  include_directories(${SFML_INCLUDE_DIR})
else()
  set(SFML_ROOT
      ""
      CACHE PATH "SFML top-level directory")
  message(
    "\n-> SFML directory not found. Set SFML_ROOT to SFML's top-level path (containing \"include\" and \"lib\" directories)."
  )
  message(
    "\n-> Make sure the SFML libraries with the same configuration (Release/Debug, Static/Dynamic) exist.\n"
  )
endif()

# Interprets an argument list separated with keywords. Example:
# parse_argument_list("MY" "FIRST;SECOND" "FIRST;a;b;c;SECOND;x;y") defines the
# following variables: MY_FIRST  = a;b;c MY_SECOND = x;y
macro(parse_argument_list PREFIX KEYWORDS ARGUMENTS)
  # Indirection over KEYWORD_LIST necessary because macro parameters are no real
  # variables.
  set(KEYWORD_LIST "${KEYWORDS}")
  set(CURRENT_KEYWORD "")

  # Parse argument list
  foreach(ARG ${ARGUMENTS})
    # See if current argument is in the list of keywords
    list(FIND KEYWORD_LIST "${ARG}" KEYWORD_FOUND)
    if(NOT KEYWORD_FOUND EQUAL -1)
      # If current element is a keyword, store it
      set(CURRENT_KEYWORD ${ARG})
    else()
      # Append current argument to last stored keyword variable
      set(${PREFIX}_${CURRENT_KEYWORD} ${${PREFIX}_${CURRENT_KEYWORD}};${ARG})
    endif()
  endforeach()
endmacro()

# Macro for subproject building Usage: build_subproject(Name SOURCES First.cpp
# Second.cpp)
macro(build_subproject SUBPROJECT_NAME)
  # Parse additional arguments (fills variables SUBPROJECT_SOURCES and
  # SUBPROJECT_DEPENDS)
  parse_argument_list("SUBPROJECT" "SOURCES" "${ARGN}")

  # Status output
  message(STATUS "-> Subproject ${SUBPROJECT_NAME}")

  # ############################################################################
  # Build executable
  project(${SUBPROJECT_NAME})
  set(SUBPROJECT_DIR "${CMAKE_SOURCE_DIR}/src/${SUBPROJECT_NAME}")
  include_directories("${CMAKE_SOURCE_DIR}/include")
  include_directories("${SUBPROJECT_DIR}/include")

  if(APPLE)
    set(SUBPROJECT_SOURCES ${SUBPROJECT_SOURCES}
                           ${CMAKE_SOURCE_DIR}/src/utils/ResourcePath.mm)
  endif()
  set(SUBPROJECT_SOURCES ${SUBPROJECT_SOURCES}
                         ${CMAKE_SOURCE_DIR}/src/utils/ResourcePath.cpp)

  if(APPLE)
    # Set some details for the app bundle. Without these my xcode crashes when
    # debugging ios :S
    set(MACOSX_BUNDLE_GUI_IDENTIFIER com.jvillasante.${SUBPROJECT_NAME})
    set(MACOSX_BUNDLE_BUNDLE_NAME ${SUBPROJECT_NAME})

    set_source_files_properties(${SUBPROJECT_DIR}/media/apple/MacIco.icns
                                PROPERTIES MACOSX_PACKAGE_LOCATION Resources)
    set_source_files_properties(${SUBPROJECT_DIR}/media
                                PROPERTIES MACOSX_PACKAGE_LOCATION Resources)
    add_executable(
      ${SUBPROJECT_NAME} MACOSX_BUNDLE
      ${SUBPROJECT_SOURCES} ${SUBPROJECT_DIR}/media/apple/MacIco.icns
      ${SUBPROJECT_DIR}/media)
    set_target_properties(${SUBPROJECT_NAME} PROPERTIES MACOSX_BUNDLE_ICON_FILE
                                                        MacIco.icns)
    set_target_properties(
      ${SUBPROJECT_NAME} PROPERTIES MACOSX_BUNDLE_INFO_PLIST
                                    ${SUBPROJECT_DIR}/media/apple/Info.plist)
    # include(BundleUtilities)
  else()
    add_executable(${SUBPROJECT_NAME} ${SUBPROJECT_SOURCES})
  endif()

  # Build static or shared libraries? Set subproject-specific DLL import macro
  if(SFML_STATIC_LIBRARIES)
    set_target_properties(${SUBPROJECT_NAME} PROPERTIES COMPILE_DEFINITIONS
                                                        "SFML_STATIC")
  endif()

  # Link SFML. For versions < 2.2, SFML_DEPENDENCIES is not defined, and we do
  # not need to link dependencies
  if(SFML_VERSION_MINOR LESS 2)
    set(SFML_DEPENDENCIES "")
  endif()

  if(APPLE)
    set(EXTRA_TARGET_LINK_LIBRARIES "-framework Foundation")
  else()
    set(EXTRA_TARGET_LINK_LIBRARIES "")
  endif()
  target_link_libraries(${SUBPROJECT_NAME} ${EXTRA_TARGET_LINK_LIBRARIES}
                        ${SFML_LIBRARIES} ${SFML_DEPENDENCIES})

  if(APPLE)
    install(
      TARGETS ${SUBPROJECT_NAME}
      BUNDLE DESTINATION ${SUBPROJECT_NAME}
      RUNTIME DESTINATION ${SUBPROJECT_NAME})

    if(NOT ${CMAKE_GENERATOR} STREQUAL "Xcode")
      set(resource_source "${SUBPROJECT_DIR}/media")
      set(resource_destination
          "${CMAKE_INSTALL_PREFIX}/${SUBPROJECT_NAME}/${SUBPROJECT_NAME}.app/Contents/Resources/media"
      )

      add_custom_command(
        TARGET ${SUBPROJECT_NAME}
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${resource_source}
                ${resource_destination} DEPENDS ${resource_destination})
    endif()
  else()
    install(
      TARGETS ${SUBPROJECT_NAME}
      DESTINATION ${SUBPROJECT_NAME}
      RUNTIME DESTINATION ${SUBPROJECT_NAME})

    install(
      DIRECTORY ${SUBPROJECT_DIR}/Media
      DESTINATION ${SUBPROJECT_NAME}
      PATTERN "CMakeLists.txt" EXCLUDE)
  endif()
endmacro()

set(INCLUDE_PATH "${CMAKE_SOURCE_DIR}/include")
set(LIBS_PATH "$ENV{HOME}/Workspace/Software/clang")

# Override install dir
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX
      "${CMAKE_SOURCE_DIR}/bin"
      CACHE PATH "default install path" FORCE)
endif()

# C++ source code, list of all subdirectories Must appear after macros,
# otherwise they are not visible in subdirectories
add_subdirectory(src/Minimal)
