cmake_minimum_required(VERSION 3.22.4)
project(App)

set(CMAKE_CXX_STANDARD 17)

set(CMAKE_CXX_FLAGS_DEBUG "-g -DDEBUG")

include_directories(headers)

file(GLOB_RECURSE SOURCES "*.cpp")
file(GLOB_RECURSE HEADERS "*.h")

add_executable(App
        App.cpp
        ExtendedTest.cpp
        ShortTest.cpp
        SortedBag.cpp
        SortedBagIterator.cpp
        ExtendedTest.h
        ShortTest.h
        SortedBag.h
        SortedBagIterator.h
)

# add_executable(${PROJECT_NAME} ${SOURCES} ${HEADERS})
# add_compile_options(-O0) 
add_compile_options(-g)

target_compile_options(${PROJECT_NAME} PRIVATE -g -Wall --coverage -fprofile-arcs -ftest-coverage)
target_link_options(${PROJECT_NAME} PRIVATE -fprofile-arcs -ftest-coverage --coverage)