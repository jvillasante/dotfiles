set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin")
include_directories(${INCLUDE_PATH})

set(PROGRAM_NAME app)
add_executable(${PROGRAM_NAME} main.cpp)
