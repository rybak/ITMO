cmake_minimum_required(VERSION 2.6)

project(orientation)


set(CMAKE_BUILD_TYPE Release)
set(sources checker.cpp)
add_definitions(-frounding-math)
link_libraries(CGAL gmp)
add_executable(orientation-checker ${sources})
