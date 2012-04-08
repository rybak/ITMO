cmake_minimum_required(VERSION 2.6)

project(convexhull-checker)

set(CMAKE_BUILD_TYPE Release)
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DDEBUG")

set(sources checker.cpp)
add_definitions(-O2 -frounding-math)
link_libraries(CGAL gmp)
add_executable(convexhull-checker ${sources})
