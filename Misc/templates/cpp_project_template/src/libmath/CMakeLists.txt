add_library(libmath
    ${CMAKE_SOURCE_DIR}/include/libmath/libmath.hpp
    libmath.cpp
)
add_library(math::libmath ALIAS libmath)
set_target_properties(libmath PROPERTIES OUTPUT_NAME math)
set_target_properties(libmath PROPERTIES LINKER_LANGUAGE CXX)

# Since we specify `PUBLIC` visibility here, any targets that compile against
# `libmath` will inherit these properties!
target_compile_features(libmath PUBLIC cxx_std_17)
target_compile_options(libmath PUBLIC
    $<$<CONFIG:Debug>:-Wall -Wextra -Werror -Wpedantic -fdiagnostics-color=always>
)

# For `./src/libmath/include` we use `PRIVATE` visibility as dependent targets
# shouldn't transitively add it to their include directories.
target_include_directories(libmath
    PUBLIC
      $<INSTALL_INTERFACE:include>
      $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
    PRIVATE
        ${CMAKE_SOURCE_DIR}/src/libmath/include
)

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
