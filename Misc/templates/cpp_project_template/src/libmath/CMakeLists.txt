add_library(libmath
    ${CMAKE_CURRENT_SOURCE_DIR}/include/libmath/libmath.hpp
    libmath.cpp
)

# Standardize the name internally and externally
add_library(math::libmath ALIAS libmath)
set_target_properties(libmath PROPERTIES OUTPUT_NAME math)

# Set the floor standard for the library and its consumers
target_compile_features(libmath PUBLIC cxx_std_17)

# Keep strict warnings PRIVATE so they don't infect consumer code!
target_compile_options(libmath PRIVATE
    $<$<CONFIG:Debug>:-Wall -Wextra -Werror -Wpedantic>
)

# Define public headers for both local builds and installed environments
target_include_directories(libmath
    PUBLIC
      $<INSTALL_INTERFACE:include>
      $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
    PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/src/libmath/include
)

# --- Installation Rules ---
include(GNUInstallDirs)

install(TARGETS libmath
    EXPORT math-targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/include/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(
    EXPORT math-targets
    FILE math-config.cmake
    NAMESPACE math::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/math
)
