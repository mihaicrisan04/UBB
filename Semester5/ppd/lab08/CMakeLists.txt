cmake_minimum_required(VERSION 3.14)
project(lab08 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")

find_package(MPI REQUIRED)

add_executable(lab08
    src/main.cpp
    src/dsm.cpp
)

target_include_directories(lab08 PRIVATE ${MPI_CXX_INCLUDE_DIRS})
target_link_libraries(lab08 PRIVATE MPI::MPI_CXX)

set_target_properties(lab08 PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)
