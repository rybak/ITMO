cmake_minimum_required(VERSION 2.6)

project(orientation)

set(sources checker.cpp)
add_definitions(-O2 -frounding-math)
link_libraries(CGAL gmp)
add_executable(orientation-checker ${sources})
