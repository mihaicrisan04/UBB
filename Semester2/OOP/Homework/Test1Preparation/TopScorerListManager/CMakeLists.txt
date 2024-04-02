cmake_minimum_required(VERSION 3.22.4)
project(App)

set(CMAKE_CXX_STANDARD 17)

set(CMAKE_CXX_FLAGS_DEBUG "-g -DDEBUG")

include_directories(headers)

file(GLOB_RECURSE SOURCES "src/*.cpp")
file(GLOB_RECURSE HEADERS "headers/*.h")

add_executable(${PROJECT_NAME} main.cpp ${SOURCES} ${HEADERS})

add_compile_options(-O0) 
add_compile_options(-g)


# Code coverage
# target_compile_options(${PROJECT_NAME} PRIVATE -g -Wall --coverage -fprofile-arcs -ftest-coverage)
# target_link_options(${PROJECT_NAME} PRIVATE -fprofile-arcs -ftest-coverage --coverage)

# Simple run
target_compile_options(${PROJECT_NAME} PRIVATE -g -Wall)
target_link_options(${PROJECT_NAME} PRIVATE -g -Wall)
