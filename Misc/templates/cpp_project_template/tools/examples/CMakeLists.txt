find_package(utils CONFIG REQUIRED)

set(PROGRAM_NAME math)
add_executable(${PROGRAM_NAME} ${PROGRAM_NAME}.cpp)
target_link_libraries(${PROGRAM_NAME} PRIVATE math::libmath)
include(GNUInstallDirs)
install(TARGETS ${PROGRAM_NAME} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

set(PROGRAM_NAME utils)
add_executable(${PROGRAM_NAME} ${PROGRAM_NAME}.cpp)
target_link_libraries(${PROGRAM_NAME} PRIVATE utils::libutils)
include(GNUInstallDirs)
install(TARGETS ${PROGRAM_NAME} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
