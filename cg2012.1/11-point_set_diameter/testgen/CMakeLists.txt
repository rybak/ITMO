cmake_minimum_required(VERSION 2.6)

project(solution)

set(CMAKE_BUILD_TYPE Release)

set(sources solution.cpp)
add_definitions(-frounding-math)
#add_definitions(-DDEBUG)
link_libraries(CGAL gmp)
add_executable(solution ${sources})
