cmake_minimum_required (VERSION 2.6)

project (segment_intersection)

add_definitions(-Wall -W  -std=c++0x)

#set(CMAKE_BUILD_TYPE Release)
set(CMAKE_BUILD_TYPE RelWithDebInfo)

add_executable (test test.cpp)
add_executable (main main.cpp geometry.cpp interval.cpp adaptive.cpp)
