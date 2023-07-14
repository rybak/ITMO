cmake_minimum_required(VERSION 2.6)

project(pointsincellgeneral-checker)

set(sources checker.cpp)
add_definitions(-O2 -frounding-math)
link_libraries(CGAL gmp)
add_executable(points_in_cell_general_checker ${sources})