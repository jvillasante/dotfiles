set(PROGRAM_NAME math)
add_executable(${PROGRAM_NAME} ${PROGRAM_NAME}.cpp)
target_link_libraries(${PROGRAM_NAME} PRIVATE math::libmath)
set_target_properties(${PROGRAM_NAME} PROPERTIES
    CXX_STANDARD 23
    CXX_STANDARD_REQUIRED ON
    CXX_EXTENSIONS OFF
)
include(GNUInstallDirs)
install(TARGETS ${PROGRAM_NAME} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

set(PROGRAM_NAME utils)
add_executable(${PROGRAM_NAME} ${PROGRAM_NAME}.cpp)
target_link_libraries(${PROGRAM_NAME} PRIVATE utils::libutils)
set_target_properties(${PROGRAM_NAME} PROPERTIES
    CXX_STANDARD 23
    CXX_STANDARD_REQUIRED ON
    CXX_EXTENSIONS OFF
)
include(GNUInstallDirs)
install(TARGETS ${PROGRAM_NAME} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
