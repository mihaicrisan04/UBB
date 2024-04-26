cmake_minimum_required(VERSION 3.22.4)
project(SortedMultiMap)

set(CMAKE_CXX_STANDARD 17)

set(CMAKE_CXX_FLAGS_DEBUG "-g -DDEBUG")

include_directories(headers)


add_executable(${PROJECT_NAME} App.cpp
                ExtendedTest.cpp
                ExtendedTest.h
                ShortTest.cpp
                ShortTest.h
                SortedMultiMap.cpp
                SortedMultiMap.h
                SMMIterator.cpp
                SMMIterator.h)

add_compile_options(-O0)
add_compile_options(-g)


# Code coverage
# target_compile_options(\${PROJECT_NAME} PRIVATE -g -Wall --coverage -fprofile-arcs -ftest-coverage)
# target_link_options(\${PROJECT_NAME} PRIVATE -fprofile-arcs -ftest-coverage --coverage)

# Simple run
target_compile_options(${PROJECT_NAME} PRIVATE -g -Wall)
target_link_options(${PROJECT_NAME} PRIVATE -g -Wall)