cmake_minimum_required(VERSION 3.14)
project(lab07 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Release by default
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Optimization flags
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")

# Find MPI
find_package(MPI REQUIRED)

# Executable
add_executable(lab07
    src/main.cpp
    src/polynomial.cpp
    src/multiply.cpp
    src/mpi_multiply.cpp
)

target_include_directories(lab07 PRIVATE ${MPI_CXX_INCLUDE_DIRS})
target_link_libraries(lab07 PRIVATE MPI::MPI_CXX)

# Set output directory
set_target_properties(lab07 PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)
