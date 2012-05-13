cmake_minimum_required(VERSION 2.6)

project(segment_rasterization-checker)

set(CMAKE_BUILD_TYPE Release)

set(sources checker.cpp)
add_definitions(-frounding-math)
#add_definitions(-DDEBUG)
link_libraries(CGAL gmp)
add_executable(segment_rasterization-checker ${sources})
