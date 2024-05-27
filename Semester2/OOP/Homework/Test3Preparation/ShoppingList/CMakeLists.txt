cmake_minimum_required(VERSION 3.22.4)
project(App)

set(CMAKE_CXX_STANDARD 17)

set(CMAKE_CXX_FLAGS_DEBUG "-g -DDEBUG")

# inlude Qt
set(CMAKE_AUTOMOC ON)
# set(CMAKE_AUTORCC ON)
# set(CMAKE_AUTOUIC ON)

set(CMAKE_PREFIX_PATH "/opt/homebrew/opt/qt")

find_package(Qt6 COMPONENTS
        Core
        Gui
        Widgets
        Charts
        REQUIRED)


# Include directories
include_directories(headers)

file(GLOB_RECURSE SOURCES "src/*.cpp")
file(GLOB_RECURSE HEADERS "headers/*.hpp")

# Add the executable
add_executable(${PROJECT_NAME} main.cpp ${SOURCES} ${HEADERS})

# compile options
# add_compile_options(-O0)
# add_compile_options(-g)


# Link the libraries of Qt
target_link_libraries(${PROJECT_NAME}
        Qt::Core
        Qt::Gui
        Qt::Widgets
        Qt::Charts
        )


