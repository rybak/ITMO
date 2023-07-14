
cmake_minimum_required(VERSION 2.6)

project(point_in_general_polygon-testgen)

set(sources testgen.cpp geometry.cpp test_case.cpp)
set(CMAKE_BUILD_TYPE Release)

add_definitions(-frounding-math -std=c++0x)
add_executable(polygon_triangulation-testgen ${sources})
