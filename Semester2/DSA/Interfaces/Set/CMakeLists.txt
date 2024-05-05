cmake_minimum_required(VERSION 3.22.4)
project(App)

set(CMAKE_CXX_STANDARD 17)

set(CMAKE_CXX_FLAGS_DEBUG "-g -DDEBUG")

include_directories(headers)

add_executable(${PROJECT_NAME} 
            App.cpp
            Set.h
            Set.cpp
            SetIterator.h
            SetIterator.cpp
            ShortTest.h
            ShortTest.cpp            
            ExtendedTest.h
            ExtendedTest.cpp
            )

add_compile_options(-O0) 
add_compile_options(-g)

# Simple run
target_compile_options(${PROJECT_NAME} PRIVATE -g -Wall)
target_link_options(${PROJECT_NAME} PRIVATE -g -Wall)
