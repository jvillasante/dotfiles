include(${PROJECT_SOURCE_DIR}/cmake/GoogleBenchmark.cmake)

set(PROGRAM_NAME bench)
add_executable(${PROGRAM_NAME} ${PROGRAM_NAME}.cpp)
target_link_libraries(${PROGRAM_NAME} PRIVATE
    benchmark::benchmark benchmark::benchmark_main)
set_target_properties(${PROGRAM_NAME} PROPERTIES
    CXX_STANDARD 23
    CXX_STANDARD_REQUIRED ON
    CXX_EXTENSIONS OFF
)
include(GNUInstallDirs)
install(TARGETS ${PROGRAM_NAME} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
