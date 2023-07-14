cmake_minimum_required(VERSION 2.6)


project(delaunay_triangulation-testgen)


set(sources testgen.cpp)
set(CMAKE_BUILD_TYPE Release)

add_definitions(-frounding-math -std=c++0x)

add_executable(delaunay_triangulation-testgen ${sources})
