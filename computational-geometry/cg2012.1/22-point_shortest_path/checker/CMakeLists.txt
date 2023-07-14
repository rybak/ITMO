cmake_minimum_required(VERSION 2.6)

project(pointshortestpath-checker)

set(sources checker.cpp)
add_definitions(-O2 -frounding-math)
link_libraries(CGAL gmp)
add_executable(point_shortest_path ${sources})