cmake_minimum_required (VERSION 2.6)

project (convex_hull)

add_definitions(-Wall -O2 -std=c++0x)

#set(CMAKE_BUILD_TYPE Release)
set(CMAKE_BUILD_TYPE RelWithDebInfo)

add_executable (main main.cpp geometry.cpp adaptive.cpp)
