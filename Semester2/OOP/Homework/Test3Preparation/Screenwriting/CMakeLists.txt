cmake_minimum_required(VERSION 3.22.4)
project(App)

set(CMAKE_CXX_STANDARD 17)

set(CMAKE_CXX_FLAGS_DEBUG "-g -DDEBUG")

# inlude Qt
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

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
# file(GLOB_RECURSE FORMS "forms/*.ui")

# Generate the headers for the UI
# qt6_wrap_ui(UI_HEADERS ${FORMS})
set(UI_HEADERS "")

# Add an option for using SQLite
option(USE_SQLITE "Use SQLite for the database" OFF)

# Add the executable
add_executable(${PROJECT_NAME} main.cpp ${SOURCES} ${HEADERS} ${FORMS} ${UI_HEADERS})

# compile options
# add_compile_options(-O0)
# add_compile_options(-g)

# SQLite
if(USE_SQLITE)
    find_package(SQLite3 REQUIRED)
    target_compile_definitions(${PROJECT_NAME} PRIVATE USE_SQLITE)
    target_link_libraries(${PROJECT_NAME} sqlite3)
endif()

# Link the libraries of Qt
target_link_libraries(${PROJECT_NAME}
        Qt::Core
        Qt::Gui
        Qt::Widgets
        Qt::Charts
        )

# Simple run
# target_compile_options(${PROJECT_NAME} PRIVATE -g -Wall)
# target_link_options(${PROJECT_NAME} PRIVATE -g -Wall)
