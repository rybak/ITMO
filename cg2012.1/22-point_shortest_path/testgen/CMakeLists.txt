cmake_minimum_required(VERSION 2.6)

project(pointshortestpath-testgen)
set(sources testgen.cpp)
add_definitions(-O2 -frounding-math)
link_libraries(CGAL gmp)
add_executable(pointshortestpath-generator ${sources})