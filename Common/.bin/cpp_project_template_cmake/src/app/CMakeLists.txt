set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin/app")
include_directories(${INCLUDE_PATH})

set(PROGRAM_NAME main)
add_executable(${PROGRAM_NAME} ${PROGRAM_NAME}.cpp)
