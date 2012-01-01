cmake_minimum_required (VERSION 2.6)

project (segment_intersection)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules")

set(CMAKE_BUILD_TYPE Release)

add_executable (test1 test1.cpp)
add_executable (main main.cpp)
