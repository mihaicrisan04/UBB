cmake_minimum_required(VERSION 3.14)
project(timetable-ga LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")

# Output directory
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Common headers
set(COMMON_HEADERS
    src/common/types.hpp
    src/common/fitness.hpp
    src/common/operators.hpp
    src/common/problem_generator.hpp
    src/common/utils.hpp
)

# ============================================
# Threaded version (always build)
# ============================================
add_executable(timetable_threaded
    src/threaded/main.cpp
)

target_include_directories(timetable_threaded PRIVATE src)

# Find threads
find_package(Threads REQUIRED)
target_link_libraries(timetable_threaded PRIVATE Threads::Threads)

# ============================================
# MPI version (optional)
# ============================================
find_package(MPI)
if(MPI_FOUND)
    add_executable(timetable_mpi
        src/mpi/main.cpp
    )
    target_include_directories(timetable_mpi PRIVATE src ${MPI_CXX_INCLUDE_DIRS})
    target_link_libraries(timetable_mpi PRIVATE MPI::MPI_CXX)
    message(STATUS "MPI found - building timetable_mpi")
else()
    message(WARNING "MPI not found - skipping timetable_mpi")
endif()

# ============================================
# OpenCL version (optional)
# ============================================
if(APPLE)
    # On macOS, OpenCL is a framework
    find_library(OPENCL_LIBRARY OpenCL)
    if(OPENCL_LIBRARY)
        add_executable(timetable_opencl
            src/opencl/main.cpp
        )
        target_include_directories(timetable_opencl PRIVATE src)
        target_link_libraries(timetable_opencl PRIVATE ${OPENCL_LIBRARY})
        message(STATUS "OpenCL found (Apple framework) - building timetable_opencl")
    else()
        message(WARNING "OpenCL not found - skipping timetable_opencl")
    endif()
else()
    find_package(OpenCL)
    if(OpenCL_FOUND)
        add_executable(timetable_opencl
            src/opencl/main.cpp
        )
        target_include_directories(timetable_opencl PRIVATE src ${OpenCL_INCLUDE_DIRS})
        target_link_libraries(timetable_opencl PRIVATE ${OpenCL_LIBRARIES})
        message(STATUS "OpenCL found - building timetable_opencl")
    else()
        message(WARNING "OpenCL not found - skipping timetable_opencl")
    endif()
endif()

# ============================================
# Print configuration
# ============================================
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "C++ standard: ${CMAKE_CXX_STANDARD}")
