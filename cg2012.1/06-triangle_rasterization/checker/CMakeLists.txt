cmake_minimum_required(VERSION 2.6)

project(trianglerasterization-checker)

set(CMAKE_BUILD_TYPE Release)

set(sources checker.cpp)
add_definitions(-frounding-math)
link_libraries(CGAL gmp)
add_executable(triangle_rasterization-checker ${sources})
